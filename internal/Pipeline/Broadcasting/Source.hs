{-# LANGUAGE GADTs #-}

module Pipeline.Broadcasting.Source where
import           Conduit
import           Lib.Logger                (MonadLogger, logError)
import           Queue.API
import           Transformers.Transformers (Transformer)

source :: (MonadLogger m , MonadIO m, Transformer t, Queue q t)
  => q
  -> ConduitT () t m ()
source q  = do
  x <- liftIO . dequeue $ q
  case x of
    Left err -> do
      lift . logError $ ("Error dequeueing: " ++ err)
    Right i -> do
      yield i
      source q

