{-# LANGUAGE RankNTypes #-}

module Pipeline.Broadcasting.API
  ( BroadcastingAPI (..)
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Lib.Logger.Types       (MonadLogger)
import           Network.ConnectionAPI  (WriterAPI)
import           Queue.API              (Queue)

newtype BroadcastingAPI = BroadcastingAPI { broadcastingRun :: forall m writer. (MonadLogger m, MonadIO m) => Int -> Queue -> WriterAPI writer -> writer -> m () }
