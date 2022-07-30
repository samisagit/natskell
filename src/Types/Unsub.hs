module Types.Unsub where

import qualified Data.ByteString as BS

data Unsub = Unsub
  { sid    :: BS.ByteString,
    maxMsg :: Maybe Int
  }

