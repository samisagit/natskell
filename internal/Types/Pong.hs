module Types.Pong where

import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           Transformers.Transformers (Transformer (..))
import           Validators.Validators

data Pong = Pong
  deriving (Eq, Show)

instance Transformer Pong where
  transform _ = LBS.fromStrict (BC.pack "PONG\r\n")

instance Validator Pong where
  validate _ = Right ()
