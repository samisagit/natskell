{-# LANGUAGE GADTs #-}

module Pipeline.Broadcasting where

import           Conduit
import           Control.Concurrent.STM
import qualified Data.ByteString           as BS
import           Lib.Logger
import           System.IO
import           Transformers.Transformers (Transformer (transform))

data QueueItem = forall m. Transformer m => QueueItem m

runPipeline :: (MonadLogger m , MonadIO m)
  => TBQueue QueueItem
  -> Handle
  -> TVar Bool
  -> m ()
runPipeline q h s = runConduit $ sourceQueue q s .| sink h

sourceQueue :: (MonadLogger m , MonadIO m)
  => TBQueue QueueItem
  -> TVar Bool
  -> ConduitT () BS.ByteString m ()
sourceQueue q s = do
  x <- liftIO . consumeUntil q $ s
  case x of
    Nothing              -> return ()
    Just (QueueItem msg) -> yield (transform msg) >> sourceQueue q s

sink :: (MonadLogger m , MonadIO m)
  => Handle
  -> ConduitT BS.ByteString Void m ()
sink h = awaitForever $ \bs -> liftIO $ BS.hPut h bs >> hFlush h

consumeUntil :: TBQueue a -> TVar Bool -> IO (Maybe a)
consumeUntil q flag = atomically loop
  where
    loop = (Just <$> readTBQueue q)
       `orElse`
           (do val <- readTVar flag
               case val of
                True -> return Nothing
                _    -> retry >> loop)

