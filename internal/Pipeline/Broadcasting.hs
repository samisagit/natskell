{-# LANGUAGE GADTs #-}

module Pipeline.Broadcasting where

import           Conduit
import           Control.Concurrent.STM
import qualified Data.ByteString           as BS
import           System.IO
import           Transformers.Transformers (Transformer (transform))

data QueueItem = forall m. Transformer m => QueueItem m

sourceQueue :: TBQueue QueueItem -> ConduitT () BS.ByteString IO ()
sourceQueue q = loop
  where
    loop = do
      QueueItem msg <- liftIO . atomically $ readTBQueue q
      yield (transform msg)
      loop

sinkHandleSafe :: Handle -> ConduitT BS.ByteString Void IO ()
sinkHandleSafe h = awaitForever $ \bs -> liftIO $ BS.hPut h bs >> hFlush h

runPipeline :: TBQueue QueueItem -> Handle -> IO ()
runPipeline q h = runConduit $ sourceQueue q .| sinkHandleSafe h
