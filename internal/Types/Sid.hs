module Sid (SIDCounter, initialSIDCounter, nextSID) where

import qualified Data.ByteString.Char8 as BC
import           Data.Word             (Word64)
import           Types

type SIDCounter = Word64

initialSIDCounter :: SIDCounter
initialSIDCounter = 0

nextSID :: SIDCounter -> (SID, SIDCounter)
nextSID counter = (BC.pack (show next), next)
  where next = counter + 1
