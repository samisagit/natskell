{-# LANGUAGE TypeApplications #-}

module Network.Connection.Core
  ( ReadError
  , WriteError
  , Transport (..)
  , TransportOption (..)
  , Conn
  , newConn
  , readData
  , closeReader
  , openReader
  , writeData
  , writeDataLazy
  , closeWriter
  , openWriter
  , openConn
  , closeConn
  , pointTransport
  , currentTransport
  , bufferRead
  , enableReadWorker
  ) where

import           Control.Concurrent       (forkIO)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Maybe               (isJust, isNothing)
import           Network.Connection.Types

newConn :: IO Conn
newConn =
  Conn
    <$> newEmptyTMVarIO
    <*> newEmptyTMVarIO
    <*> newEmptyTMVarIO
    <*> newTVarIO mempty
    <*> newTBQueueIO 1000
    <*> newTVarIO False
    <*> newTVarIO False

readChunkSize :: Int
readChunkSize = 4096

startReadWorker :: Conn -> IO ()
startReadWorker conn = do
  shouldStart <- atomically $ do
    enabled <- readTVar (readWorkerEnabled conn)
    running <- readTVar (readWorkerRunning conn)
    if not enabled || running
      then return False
      else do
        writeTVar (readWorkerRunning conn) True
        return True
  when shouldStart . void $ forkIO (readWorkerLoop conn)

readWorkerLoop :: Conn -> IO ()
readWorkerLoop conn = do
  let cleanup = atomically $ writeTVar (readWorkerRunning conn) False
  finally loop cleanup
  where
    loop = do
      (blocked, enabled) <- atomically $ do
        blocked <- tryReadTMVar (readBlock conn)
        enabled <- readTVar (readWorkerEnabled conn)
        return (blocked, enabled)
      case (blocked, enabled) of
        (Just _, _) -> return ()
        (_, False)  -> return ()
        _ -> do
          current <- currentTransport conn
          case current of
            Nothing -> return ()
            Just currentTransport' -> do
              result <- try @SomeException (transportRead currentTransport' readChunkSize)
              shouldContinue <- enqueueReadResult conn result
              case result of
                Left _  -> return ()
                Right _ -> when shouldContinue loop

enqueueReadResult :: Conn -> Either SomeException ByteString -> IO Bool
enqueueReadResult conn result = atomically $
  (do
      blocked <- tryReadTMVar (readBlock conn)
      check (isNothing blocked)
      writeTBQueue (readQueue conn) (either (Left . show) Right result)
      return True)
  `orElse`
  (do
      _ <- readTMVar (readBlock conn)
      return False)

enableReadWorker :: Conn -> IO ()
enableReadWorker conn = atomically $ writeTVar (readWorkerEnabled conn) True

readData :: Conn -> Int -> IO (Either ReadError ByteString)
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
          current <- currentTransport conn
          case current of
            Nothing -> return $ Left "Transport not initialized"
            Just currentTransport' -> do
              enabled <- readTVarIO (readWorkerEnabled conn)
              if enabled
                then do
                  startReadWorker conn
                  result <- atomically $
                    (Left "Read operation is blocked" <$ readTMVar (readBlock conn))
                    `orElse`
                    readTBQueue (readQueue conn)
                  case result of
                    Left err -> return $ Left err
                    Right bytes -> do
                      let (chunk, rest) = BS.splitAt n bytes
                      unless (BS.null rest)
                        (atomically $ modifyTVar' (readBuffer conn) (<> rest))
                      return $ Right chunk
                else do
                  resultVar <- newEmptyTMVarIO
                  _ <- forkIO $ do
                    result <- try @SomeException (transportRead currentTransport' n)
                    atomically . void $ tryPutTMVar resultVar result
                  result <- atomically $
                    (Left "Read operation is blocked" <$ readTMVar (readBlock conn))
                    `orElse`
                    (Right <$> readTMVar resultVar)
                  case result of
                    Left err -> return $ Left err
                    Right (Left err) -> return $ Left (show err)
                    Right (Right bytes) -> do
                      let (chunk, rest) = BS.splitAt n bytes
                      unless (BS.null rest)
                        (atomically $ modifyTVar' (readBuffer conn) (<> rest))
                      return $ Right chunk

closeReader :: Conn -> IO ()
closeReader conn = void . atomically $ tryPutTMVar (readBlock conn) ()

openReader :: Conn -> IO ()
openReader conn = void . atomically $ tryTakeTMVar (readBlock conn)

writeData :: Conn -> ByteString -> IO (Either WriteError ())
writeData conn bytes = do
  blocked <- atomically $ tryReadTMVar (writeBlock conn)
  if isJust blocked
    then return $ Left "Write operation is blocked"
    else do
      current <- currentTransport conn
      case current of
        Nothing -> return $ Left "Transport not initialized"
        Just currentTransport' -> do
          result <- try @SomeException $ do
            transportWrite currentTransport' bytes
            transportFlush currentTransport'
          case result of
            Left err -> return $ Left (show err)
            Right _  -> return $ Right ()

writeDataLazy :: Conn -> LBS.ByteString -> IO (Either WriteError ())
writeDataLazy conn bytes = do
  blocked <- atomically $ tryReadTMVar (writeBlock conn)
  if isJust blocked
    then return $ Left "Write operation is blocked"
    else do
      current <- currentTransport conn
      case current of
        Nothing -> return $ Left "Transport not initialized"
        Just currentTransport' -> do
          result <- try @SomeException $ do
            transportWriteLazy currentTransport' bytes
            transportFlush currentTransport'
          case result of
            Left err -> return $ Left (show err)
            Right _  -> return $ Right ()

closeWriter :: Conn -> IO ()
closeWriter conn = void . atomically $ tryPutTMVar (writeBlock conn) ()

openWriter :: Conn -> IO ()
openWriter conn = void . atomically $ tryTakeTMVar (writeBlock conn)

pointTransport :: Conn -> Transport -> IO ()
pointTransport conn newTransport = atomically $ do
  _ <- tryTakeTMVar (transport conn)
  putTMVar (transport conn) newTransport

currentTransport :: Conn -> IO (Maybe Transport)
currentTransport conn = atomically $ tryReadTMVar (transport conn)

closeConn :: Conn -> IO ()
closeConn conn = do
  closeReader conn
  closeWriter conn
  current <- currentTransport conn
  case current of
    Nothing -> return ()
    Just currentTransport' -> void $ try @SomeException (transportClose currentTransport')

openConn :: Conn -> IO ()
openConn conn = do
  openReader conn
  openWriter conn
  atomically $ do
    writeTVar (readBuffer conn) mempty
    void $ flushTBQueue (readQueue conn)
    writeTVar (readWorkerEnabled conn) False

bufferRead :: Conn -> ByteString -> IO ()
bufferRead conn bytes =
  unless (BS.null bytes)
    (atomically $ modifyTVar' (readBuffer conn) (bytes <>))
