module Pipeline.Broadcasting.Transformer where

import           Conduit
import qualified Data.ByteString           as BS
import           Lib.Logger
import           Transformers.Transformers

transformer :: (MonadLogger m, MonadIO m, Transformer t)
  => ConduitT t BS.ByteString m ()
transformer = awaitForever $ \t -> yield (transform t)
