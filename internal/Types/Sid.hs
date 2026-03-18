module Sid
  ( module Sid.Types
  , initialSIDCounter
  , nextSID
  , sidApi
  ) where

import qualified Data.ByteString.Char8 as BC
import           Sid.Types
import           SidAPI                (SidAPI (..))
import           Types

initialSIDCounter :: SIDCounter
initialSIDCounter = 0

nextSID :: SIDCounter -> (SID, SIDCounter)
nextSID counter = (BC.pack (show next), next)
  where next = counter + 1

sidApi :: SidAPI
sidApi = SidAPI
  { sidInitial = initialSIDCounter
  , sidNext = nextSID
  }
