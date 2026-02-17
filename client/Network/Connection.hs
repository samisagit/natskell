{-# LANGUAGE TypeApplications #-}

module Network.Connection where

import           Control.Concurrent        (forkIO, killThread)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.Maybe
import           Network.API
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.TLS               as TLS

data Transport = Transport
                   { transportRead :: Int -> IO ByteString
                   , transportWrite :: ByteString -> IO ()
                   , transportFlush :: IO ()
                   , transportClose :: IO ()
                   , transportUpgrade :: Maybe (TLS.ClientParams -> IO (Either String Transport))
                   }

data Conn = Conn
              { transport  :: TMVar Transport
              , readBlock  :: TMVar ()
              , writeBlock :: TMVar ()
              , readBuffer :: TVar ByteString
              }

newConn :: IO Conn
newConn =
  Conn <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newTVarIO mempty

tcpTransport :: NS.Socket -> Transport
tcpTransport sock =
  Transport
    { transportRead = NSB.recv sock
    , transportWrite = NSB.sendAll sock
    , transportFlush = pure ()
    , transportClose = NS.close sock
    , transportUpgrade = Just (upgradeTcp sock)
    }

tlsTransport :: TLS.Context -> Transport
tlsTransport ctx =
  Transport
    { transportRead = const (TLS.recvData ctx)
    , transportWrite = TLS.sendData ctx . BSL.fromStrict
    , transportFlush = TLS.contextFlush ctx
    , transportClose = do
        void $ try @SomeException (TLS.bye ctx)
        void $ try @SomeException (TLS.contextClose ctx)
    , transportUpgrade = Nothing
    }

upgradeTcp :: NS.Socket -> TLS.ClientParams -> IO (Either String Transport)
upgradeTcp sock params = do
  let backend = TLS.Backend
        { TLS.backendSend = NSB.sendAll sock
        , TLS.backendRecv = NSB.recv sock
        , TLS.backendFlush = pure ()
        , TLS.backendClose = NS.close sock
        }
  result <- try @SomeException $ do
    ctx <- TLS.contextNew backend params
    TLS.handshake ctx
    pure (tlsTransport ctx)
  case result of
    Left err        -> return $ Left (show err)
    Right transport -> return $ Right transport

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
              Nothing               -> return $ Left "Transport not initialized"
              Just currentTransport -> readFromTransport currentTransport
    where
      readFromTransport currentTransport = do
        resultVar <- newEmptyTMVarIO
        readerThread <- forkIO $ do
          r <- try @SomeException (transportRead currentTransport n)
          atomically $ putTMVar resultVar r

        outcome <- atomically $
          (Left <$> takeTMVar (readBlock conn)) `orElse`
          (Right <$> takeTMVar resultVar)

        killThread readerThread

        case outcome of
          Left _           -> return $ Left "Read aborted due to block"
          Right (Left err) -> return $ Left (show err)
          Right (Right bytes) -> do
            let (chunk, rest) = BS.splitAt n bytes
            unless (BS.null rest)
              (atomically $ modifyTVar' (readBuffer conn) (<> rest))
            return $ Right chunk

  closeReader conn = void . atomically $ tryPutTMVar (readBlock conn) ()
  openReader conn = void . atomically $ tryTakeTMVar (readBlock conn)

instance ConnectionWriter Conn where
  writeData conn bytes = do
    isBlocked <- atomically $ tryReadTMVar (writeBlock conn)
    if isJust isBlocked
      then return $ Left "Write operation is blocked"
      else do
        current <- atomically $ tryReadTMVar (transport conn)
        case current of
          Nothing -> return $ Left "Transport not initialized"
          Just currentTransport -> do
            result <- try @SomeException $ do
              transportWrite currentTransport bytes
              transportFlush currentTransport
            case result of
              Left err -> return $ Left (show err)
              Right _  -> return $ Right ()

  closeWriter conn = void . atomically $ tryPutTMVar (writeBlock conn) ()
  openWriter conn = void . atomically $ tryTakeTMVar (writeBlock conn)

pointTransport :: Conn -> Transport -> IO ()
pointTransport conn newTransport = atomically $ do
  _ <- tryTakeTMVar (transport conn)
  putTMVar (transport conn) newTransport

close :: Conn -> IO ()
close conn = do
  closeReader conn
  closeWriter conn
  current <- atomically $ tryReadTMVar (transport conn)
  case current of
    Nothing -> return ()
    Just currentTransport -> void $ try @SomeException (transportClose currentTransport)

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
    Nothing -> return $ Left "Transport not initialized"
    Just currentTransport ->
      case transportUpgrade currentTransport of
        Nothing -> return $ Right ()
        Just upgrade -> do
          result <- upgrade params
          case result of
            Left err -> return $ Left err
            Right newTransport -> do
              pointTransport conn newTransport
              return $ Right ()
