module Pipeline.Broadcasting.Transformer where

import           Conduit
import qualified Data.ByteString.Lazy      as LBS
import           Lib.Logger.Types          (LogLevel (..), MonadLogger (..))
import           Transformers.Transformers

transformer :: (MonadLogger m, MonadIO m, Transformer t)
  => Int
  -> ConduitT t LBS.ByteString m ()
transformer bufferLimit = awaitForever $ \t -> do
  let bytes = transform t
      byteCount = fromIntegral (LBS.length bytes)
  if byteCount > bufferLimit
    then
      lift . logMessage Error $
        "dropping outbound message: " ++ show byteCount ++ " bytes exceeds limit " ++ show bufferLimit
    else yield bytes
