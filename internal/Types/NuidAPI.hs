module NuidAPI
  ( Nuid
  , NuidAPI (..)
  , nuidApi
  ) where

import           Data.ByteString (ByteString)
import           Nuid            (Nuid, newNuidIO, nextNuid)

-- | API wrapper for NUID capabilities.
data NuidAPI = NuidAPI
                 { nuidNew  :: IO Nuid
                 , nuidNext :: Nuid -> (ByteString, Nuid)
                 }

nuidApi :: NuidAPI
nuidApi = NuidAPI
  { nuidNew = newNuidIO
  , nuidNext = nextNuid
  }
