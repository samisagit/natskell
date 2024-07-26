{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Buffer.Buffer (withBuffer) where

import           Control.Concurrent        (threadDelay, forkIO)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State
import qualified Data.ByteString           as BS
import Control.Monad (void)

data BufferStatus = EMPTY | FULL | PARTIAL
type Puller = Int -> IO (Maybe BS.ByteString)
type Consumer = BS.ByteString -> IO Int
type BackOff = Int
data Buffer = Buffer {
  bytes::BS.ByteString,
  status::BufferStatus,
  puller::Puller,
  consumer::Consumer,
  backOff::BackOff
  }

defaultLimit = 1024
defaultBackOffMultiplier = 100000

-- TODO: there is no concurrency safety here, we need to lock the buffer somehow

withBuffer :: Puller -> Consumer -> IO ()
withBuffer puller consumer = do
  let buf = Buffer BS.empty EMPTY puller consumer 0
  void . (forkIO . void) . liftIO . runStateT loop $ buf

loop :: StateT Buffer IO BS.ByteString
loop = do
  consume
  hydrate
  loop

consume :: StateT Buffer IO BS.ByteString
consume = do
  buf <- get
  used <- liftIO . consumer buf $ bytes buf
  chomp used

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
    Just "" -> do
      liftIO . threadDelay $ backOff buf * defaultBackOffMultiplier
      put $ buf {backOff=incrementBackOffMultiplier $ backOff buf}
      handleEmpty
    Just newBS -> do
      put $ buf {status=statusFromBS newBS, bytes=BS.append (bytes buf) newBS, backOff=0}
      return newBS
    Nothing -> do
      liftIO . threadDelay $ backOff buf * defaultBackOffMultiplier
      put $ buf {backOff=incrementBackOffMultiplier $ backOff buf}
      handleEmpty

handlePartial :: StateT Buffer IO BS.ByteString
handlePartial = do
  buf <- get
  result <- liftIO $ puller buf (defaultLimit - BS.length (bytes buf))
  case result of
    Just "" -> do
      liftIO . threadDelay $ backOff buf * defaultBackOffMultiplier
      put $ buf {backOff=incrementBackOffMultiplier $ backOff buf}
      handlePartial
    Just newBS -> do
      put $ buf {status=statusFromBS newBS, bytes=BS.append (bytes buf) newBS, backOff=0}
      return newBS
    Nothing -> do
      liftIO . threadDelay $ backOff buf * defaultBackOffMultiplier
      put $ buf {backOff=incrementBackOffMultiplier $ backOff buf}
      handlePartial

handleFull :: StateT Buffer IO BS.ByteString
handleFull = do
  buf <- get
  if BS.length (bytes buf) == defaultLimit then do
    liftIO . threadDelay $ backOff buf * defaultBackOffMultiplier
    put $ buf {backOff=incrementBackOffMultiplier $ backOff buf}
    handleFull
  else do
    put $ buf {status=PARTIAL}
    handlePartial

statusFromBS :: BS.ByteString -> BufferStatus
statusFromBS bs
  | BS.null bs = EMPTY
  | BS.length bs == defaultLimit = FULL
  | otherwise = PARTIAL

incrementBackOffMultiplier :: Int -> Int
incrementBackOffMultiplier current = if current == 10 then 10 else current + 1
