module Validators.Validators where

import           Data.ByteString

-- TODO: should make this an either ByteString () to make the failure case more useful
-- for application/monad use
class Validator a where
  validate :: a -> Maybe ByteString

