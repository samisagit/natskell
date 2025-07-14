{-# LANGUAGE GADTs #-}

module Pipeline.Broadcasting where

import           Conduit
import           Control.Concurrent.STM
import qualified Data.ByteString           as BS
import           Pipeline.Status
import           System.IO
import           Transformers.Transformers (Transformer (transform))

data QueueItem = forall m. Transformer m => QueueItem m

runPipeline :: TBQueue QueueItem -> Handle -> TVar Status -> IO ()
runPipeline q h s = runConduit $ sourceQueue q s .| sink h

sourceQueue :: TBQueue QueueItem -> TVar Status -> ConduitT () BS.ByteString IO ()
sourceQueue q s = do
  x <- liftIO . consumeUntil q $ s
  case x of
    Nothing              -> return ()
    Just (QueueItem msg) -> yield (transform msg) >> sourceQueue q s

sink :: Handle -> ConduitT BS.ByteString Void IO ()
sink h = awaitForever $ \bs -> liftIO $ BS.hPut h bs >> hFlush h

consumeUntil :: TBQueue a -> TVar Status -> IO (Maybe a)
consumeUntil q flag = atomically loop
  where
    loop = (Just <$> readTBQueue q)
       `orElse`
           (do val <- readTVar flag
               case val of
                Disconnected _  -> return Nothing
                Disconnecting _ -> return Nothing
                Draining        -> return Nothing
                _               -> retry >> loop)
