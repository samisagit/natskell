module SidAPI
  ( SIDCounter
  , SID
  , SidAPI (..)
  , sidApi
  ) where

import           Sid       (SIDCounter, initialSIDCounter, nextSID)
import           Types.Msg (SID)

-- | API wrapper for SID capabilities.
data SidAPI = SidAPI
                { sidInitial :: SIDCounter
                , sidNext    :: SIDCounter -> (SID, SIDCounter)
                }

sidApi :: SidAPI
sidApi = SidAPI
  { sidInitial = initialSIDCounter
  , sidNext = nextSID
  }
