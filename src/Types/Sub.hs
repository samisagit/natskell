module Types.Sub where

import qualified Data.ByteString as BS

data Sub = Sub
  { subject    :: BS.ByteString,
    queueGroup :: Maybe BS.ByteString,
    sid        :: BS.ByteString
  }
