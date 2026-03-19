module SidAPI
  ( SIDCounter
  , SID
  , SidAPI (..)
  ) where

import           Sid.Types (SIDCounter)
import           Types.Msg (SID)

-- | API wrapper for SID capabilities.
data SidAPI = SidAPI
                { sidInitial :: SIDCounter
                , sidNext    :: SIDCounter -> (SID, SIDCounter)
                }
