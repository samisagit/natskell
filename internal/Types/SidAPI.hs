module SidAPI
  ( SIDCounter
  , SidAPI (..)
  ) where

import           Sid.Types (SIDCounter)
import           Types     (SID)

-- | API wrapper for SID capabilities.
data SidAPI = SidAPI
                { sidInitial :: SIDCounter
                , sidNext    :: SIDCounter -> (SID, SIDCounter)
                }
