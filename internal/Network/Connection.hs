{-# LANGUAGE TypeApplications #-}

module Network.Connection where

import           Control.Concurrent     (forkIO, killThread)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString
import           Data.Maybe
import           GHC.IO.Handle
import           Network.API

data Conn = Conn
              { socket     :: TMVar Handle
              , readBlock  :: TMVar ()
              , writeBlock :: TMVar ()
              }

instance ConnectionReader Conn where
  readData conn n = do
    blocked <- atomically $ tryReadTMVar (readBlock conn)
    if isJust blocked
      then return $ Left "Read operation is blocked"
      else do
        mc <- atomically $ tryReadTMVar (socket conn)
        case mc of
          Nothing -> return $ Left "Socket not pointed"
          Just c -> do
            resultVar <- newEmptyTMVarIO
            readerThread <- forkIO $ do
              r <- try @SomeException (hGetSome c n)
              atomically $ putTMVar resultVar r

            -- Wait for either the read to finish or the blocker to trigger
            outcome <- atomically $
              (Left <$> takeTMVar (readBlock conn)) `orElse`
              (Right <$> takeTMVar resultVar)

            -- Clean up reader thread if still running
            killThread readerThread

            -- Interpret outcome
            case outcome of
              Left _              -> return $ Left "Read aborted due to block"
              Right (Left err)    -> return $ Left (show err)
              Right (Right bytes) -> return $ Right bytes

  closeReader conn = void . atomically $ tryPutTMVar (readBlock conn) ()

instance ConnectionWriter Conn where
  writeData conn bytes = do
    isBlocked <- atomically $ tryReadTMVar (writeBlock conn)
    if isJust isBlocked
      then return $ Left "Write operation is blocked"
      else do
        c <- atomically $ tryReadTMVar (socket conn)
        case c of
          Nothing -> return $ Left "Socket not pointed"
          Just c -> do
            result <- try @SomeException (hPut c bytes >> hFlush c)
            case result of
              Left err -> return $ Left (show err)
              Right _  -> return $ Right ()

  closeWriter conn = void . atomically $ tryPutTMVar (writeBlock conn) ()

point :: Conn -> Handle -> IO ()
point c h = do
  atomically $ writeTMVar (socket c) h
  return ()

close :: Conn -> IO ()
close conn = do
  closeReader conn
  closeWriter conn
  c <- atomically $ tryReadTMVar (socket conn)
  case c of
    Nothing -> return ()
    Just c -> do
      hFlush c
      hClose c

