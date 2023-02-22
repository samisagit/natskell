module Types.Pong where

import           Validators.Validators

data Pong = Pong deriving (Show, Eq)

instance Validator Pong where
  validate _ = Right ()
