module Types.Pong where

import           Validators.Validators

data Pong = Pong
  deriving (Eq, Show)

instance Validator Pong where
  validate _ = Right ()
