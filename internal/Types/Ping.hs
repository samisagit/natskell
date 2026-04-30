module Types.Ping where

import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           Transformers.Transformers (Transformer (..))
import           Validators.Validators

data Ping = Ping
  deriving (Eq, Show)

instance Transformer Ping where
  transform _ = LBS.fromStrict (BC.pack "PING\r\n")

instance Validator Ping where
  validate _ = Right ()
