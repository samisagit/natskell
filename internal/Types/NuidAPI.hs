module NuidAPI
  ( Nuid
  , NuidAPI (..)
  ) where

import           Data.ByteString (ByteString)
import           Nuid.Types      (Nuid)

-- | API wrapper for NUID capabilities.
data NuidAPI = NuidAPI
                 { nuidNew  :: IO Nuid
                 , nuidNext :: Nuid -> (ByteString, Nuid)
                 }
