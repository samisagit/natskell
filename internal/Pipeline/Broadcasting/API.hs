{-# LANGUAGE RankNTypes #-}

module Pipeline.Broadcasting.API
  ( BroadcastingAPI (..)
  ) where

import           Control.Monad.IO.Class    (MonadIO)
import           Lib.Logger.Types          (MonadLogger)
import           Network.ConnectionAPI     (WriterAPI)
import           Queue.API                 (Queue)
import           Transformers.Transformers (Transformer)

newtype BroadcastingAPI = BroadcastingAPI { broadcastingRun :: forall m q t writer. (MonadLogger m, MonadIO m, Transformer t, Queue q t) => Int -> q -> WriterAPI writer -> writer -> m () }
