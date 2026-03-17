module Transformers.Types
  ( Transformer (..)
  ) where

import qualified Data.ByteString.Lazy as LBS

class Transformer a where
  transform :: a -> LBS.ByteString
