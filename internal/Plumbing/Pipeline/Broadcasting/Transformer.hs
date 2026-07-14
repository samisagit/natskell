module Pipeline.Broadcasting.Transformer where

import           Conduit
import qualified Data.ByteString.Lazy      as LBS
import           Lib.Logger.Types          (MonadLogger)
import           Transformers.Transformers

transformer :: (MonadLogger m, MonadIO m, Transformer t)
  => ConduitT t LBS.ByteString m ()
transformer = awaitForever (yield . transform)
