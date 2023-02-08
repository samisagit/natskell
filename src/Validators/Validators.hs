module Validators.Validators where

import           Data.ByteString

class Validator a where
  validate :: a -> Maybe ByteString


