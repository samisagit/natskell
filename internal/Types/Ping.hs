module Types.Ping where

import           Validators.Validators

data Ping = Ping deriving (Show, Eq)

instance Validator Ping where
  validate _ = Right ()
