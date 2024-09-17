{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Buffer.Buffer (newBuffer, hydrate, chomp, Buffer(bytes)) where

import           Control.Concurrent        (threadDelay)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State
import qualified Data.ByteString           as BS

data BufferStatus = EMPTY | FULL | PARTIAL
type Puller = Int -> IO BS.ByteString
type BackOff = Int
data Buffer = Buffer {
  bytes   :: BS.ByteString,
  status  :: BufferStatus,
  puller  :: Puller,
  backOff :: BackOff
  }

defaultLimit = 1024
defaultBackOffMultiplier = 100000

newBuffer :: Puller -> Buffer
newBuffer puller = do
  Buffer BS.empty EMPTY puller 0

chomp :: Int -> StateT Buffer IO BS.ByteString
chomp count = do
  buf <- get
  let bs = BS.drop count (bytes buf)
  put $ buf {bytes=bs}
  return bs

hydrate :: StateT Buffer IO BS.ByteString
hydrate = do
  buf <- get
  case status buf of
    EMPTY   -> handleEmpty
    PARTIAL -> handlePartial
    FULL    -> handleFull

handleEmpty :: StateT Buffer IO BS.ByteString
handleEmpty = do
  buf <- get
  result <- liftIO $ puller buf defaultLimit
  case result of
    "" -> do
      liftIO . delay $ backOff buf
      put $ buf {backOff=incrementBackOffMultiplier $ backOff buf}
      bytes <$> get
    newBS -> do
      put $ buf {status=statusFromBS newBS, bytes=BS.append (bytes buf) newBS, backOff=0}
      bytes <$> get

handlePartial :: StateT Buffer IO BS.ByteString
handlePartial = do
  buf <- get
  result <- liftIO $ puller buf (defaultLimit - BS.length (bytes buf))
  case result of
    "" -> do
      liftIO . delay $ backOff buf
      put $ buf {backOff=incrementBackOffMultiplier $ backOff buf}
      bytes <$> get
    newBS -> do
      put $ buf {status=statusFromBS newBS, bytes=BS.append (bytes buf) newBS, backOff=0}
      bytes <$> get

handleFull :: StateT Buffer IO BS.ByteString
handleFull = do
  buf <- get
  if BS.length (bytes buf) == defaultLimit then do
    liftIO . delay $ backOff buf
    put $ buf {backOff=incrementBackOffMultiplier $ backOff buf}
    bytes <$> get
  else do
    put $ buf {status=PARTIAL}
    bytes <$> get

statusFromBS :: BS.ByteString -> BufferStatus
statusFromBS bs
  | BS.null bs = EMPTY
  | BS.length bs == defaultLimit = FULL
  | otherwise = PARTIAL

incrementBackOffMultiplier :: Int -> Int
incrementBackOffMultiplier current = if current == 100 then 100 else current + 1

delay :: Int -> IO ()
delay backoff = do
  threadDelay nano
  where modulus = backoff `mod` 10
        nano = ((backoff - modulus) `div` 10) * defaultBackOffMultiplier

--ld :: String -> IO a -> IO a
--ld name action = do
--  currentTime <- getCurrentTime
--  res <- action
--  completedTime <- getCurrentTime
--  print $ name ++ ": " ++ show (diffUTCTime completedTime currentTime)
--  return res
