{-# LANGUAGE TypeApplications #-}

module Network.Connection where

import           Control.Concurrent     (forkIO, killThread)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Maybe
import           GHC.IO.Handle
import           Network.API
import qualified Network.TLS            as TLS

data ConnTransport = ConnHandle Handle
                   | ConnTLS TLS.Context

data Conn = Conn
              { transport  :: TMVar ConnTransport
              , readBlock  :: TMVar ()
              , writeBlock :: TMVar ()
              , readBuffer :: TVar ByteString
              }

instance ConnectionReader Conn where
  readData conn n = do
    blocked <- atomically $ tryReadTMVar (readBlock conn)
    if isJust blocked
      then return $ Left "Read operation is blocked"
      else do
        buffered <- atomically $ do
          buf <- readTVar (readBuffer conn)
          if BS.null buf
            then return Nothing
            else do
              let (chunk, rest) = BS.splitAt n buf
              writeTVar (readBuffer conn) rest
              return (Just chunk)
        case buffered of
          Just chunk -> return $ Right chunk
          Nothing -> do
            mc <- atomically $ tryReadTMVar (transport conn)
            case mc of
              Nothing -> return $ Left "Socket not pointed"
              Just c  -> readFromTransport c
    where
      readFromTransport transport = do
        resultVar <- newEmptyTMVarIO
        readerThread <- forkIO $ do
          r <- try @SomeException $ case transport of
            ConnHandle h -> BS.hGetSome h n
            ConnTLS ctx  -> TLS.recvData ctx
          atomically $ putTMVar resultVar r

        outcome <- atomically $
          (Left <$> takeTMVar (readBlock conn)) `orElse`
          (Right <$> takeTMVar resultVar)

        killThread readerThread

        case outcome of
          Left _           -> return $ Left "Read aborted due to block"
          Right (Left err) -> return $ Left (show err)
          Right (Right bytes) ->
            case transport of
              ConnHandle _ -> return $ Right bytes
              ConnTLS _ ->
                if BS.length bytes <= n
                  then return $ Right bytes
                  else do
                    let (chunk, rest) = BS.splitAt n bytes
                    atomically $ modifyTVar' (readBuffer conn) (<> rest)
                    return $ Right chunk

  closeReader conn = void . atomically $ tryPutTMVar (readBlock conn) ()
  openReader conn = void . atomically $ tryTakeTMVar (readBlock conn)

instance ConnectionWriter Conn where
  writeData conn bytes = do
    isBlocked <- atomically $ tryReadTMVar (writeBlock conn)
    if isJust isBlocked
      then return $ Left "Write operation is blocked"
      else do
        c <- atomically $ tryReadTMVar (transport conn)
        case c of
          Nothing -> return $ Left "Socket not pointed"
          Just transport -> do
            result <- try @SomeException $ case transport of
              ConnHandle h -> BS.hPut h bytes >> hFlush h
              ConnTLS ctx  -> TLS.sendData ctx (BSL.fromStrict bytes) >> TLS.contextFlush ctx
            case result of
              Left err -> return $ Left (show err)
              Right _  -> return $ Right ()

  closeWriter conn = void . atomically $ tryPutTMVar (writeBlock conn) ()
  openWriter conn = void . atomically $ tryTakeTMVar (writeBlock conn)

point :: Conn -> Handle -> IO ()
point c h =
  pointTransport c (ConnHandle h)

pointTransport :: Conn -> ConnTransport -> IO ()
pointTransport c newTransport = atomically $ do
  _ <- tryTakeTMVar (transport c)
  putTMVar (transport c) newTransport

close :: Conn -> IO ()
close conn = do
  closeReader conn
  closeWriter conn
  c <- atomically $ tryReadTMVar (transport conn)
  case c of
    Nothing -> return ()
    Just transport -> do
      case transport of
        ConnHandle h -> do
          hFlush h
          hClose h
        ConnTLS ctx -> do
          void $ try @SomeException (TLS.bye ctx)
          void $ try @SomeException (TLS.contextClose ctx)

open :: Conn -> IO ()
open conn = do
  openReader conn
  openWriter conn
  atomically $ writeTVar (readBuffer conn) mempty

bufferRead :: Conn -> ByteString -> IO ()
bufferRead conn bytes =
  unless (BS.null bytes)
    (atomically $ modifyTVar' (readBuffer conn) (bytes <>))

upgradeToTLS :: Conn -> TLS.ClientParams -> IO (Either String ())
upgradeToTLS conn params = do
  current <- atomically $ tryReadTMVar (transport conn)
  case current of
    Nothing -> return $ Left "Socket not pointed"
    Just (ConnTLS _) -> return $ Right ()
    Just (ConnHandle h) -> do
      let backend = TLS.Backend
            { TLS.backendSend = BS.hPut h
            , TLS.backendRecv = BS.hGetSome h
            , TLS.backendFlush = hFlush h
            , TLS.backendClose = hClose h
            }
      result <- try @SomeException $ do
        ctx <- TLS.contextNew backend params
        TLS.handshake ctx
        pointTransport conn (ConnTLS ctx)
      case result of
        Left err -> return $ Left (show err)
        Right _  -> return $ Right ()
