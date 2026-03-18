{-# LANGUAGE RankNTypes #-}

module Transformers.TransformersAPI
  ( TransformersAPI (..)
  ) where

import           Data.ByteString.Lazy (ByteString)
import           Transformers.Types

-- | API wrapper for transformer capabilities.
newtype TransformersAPI = TransformersAPI { transformersTransform :: forall a. Transformer a => a -> ByteString }
