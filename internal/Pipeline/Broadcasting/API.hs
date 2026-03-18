{-# LANGUAGE RankNTypes #-}

module Pipeline.Broadcasting.API
  ( BroadcastingAPI (..)
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Lib.Logger.Types       (MonadLogger)
import           Network.API            (ConnectionWriter)
import           Queue.API              (Queue)
import           Transformers.Types     (Transformer)

newtype BroadcastingAPI = BroadcastingAPI { broadcastingRun :: forall m q t writer. (MonadLogger m, MonadIO m, ConnectionWriter writer, Transformer t, Queue q t) => Int -> q -> writer -> m () }
