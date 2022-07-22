module Types.Unsub where

data Unsub = Unsub
  { sid    :: String,
    maxMsg :: Maybe Int
  }

