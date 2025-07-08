{-# LANGUAGE GADTs #-}

module Pipeline.Broadcasting where

import           Conduit
import           Control.Concurrent.STM
import qualified Data.ByteString           as BS
import           Pipeline.Connection
import           System.IO
import           Transformers.Transformers

sourceQueue :: Connection err result -> ConduitT () BS.ByteString IO ()
sourceQueue conn = loop
  where
    q = queue conn
    loop = do
      QueueItem msg <- liftIO . atomically $ readTBQueue q
      yield (transform msg)
      loop

sinkHandleSafe :: Connection err result -> ConduitT BS.ByteString Void IO ()
sinkHandleSafe conn = do
  awaitForever $ \bs -> liftIO $ BS.hPut (h conn) bs >> hFlush (h conn)

runPipeline :: Connection a b -> IO ()
runPipeline conn = do
  runConduit $ sourceQueue conn .| sinkHandleSafe conn
