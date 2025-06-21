module Types.Ping where

import           Validators.Validators

data Ping = Ping
  deriving (Eq, Show)

instance Validator Ping where
  validate _ = Right ()
