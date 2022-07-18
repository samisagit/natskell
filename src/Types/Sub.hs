module Types.Sub where

data Sub = Sub
  { subject    :: String,
    queueGroup :: Maybe String,
    sid        :: String
  }
