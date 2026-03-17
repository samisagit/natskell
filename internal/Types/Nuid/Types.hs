module Nuid.Types
  ( Nuid (..)
  ) where

import qualified Data.ByteString as BS
import           Data.Word       (Word64)
import           System.Random   (StdGen)

data Nuid = Nuid
              { nuidPrefix :: BS.ByteString
              , nuidSeq    :: Word64
              , nuidInc    :: Word64
              , nuidRng    :: StdGen
              }
