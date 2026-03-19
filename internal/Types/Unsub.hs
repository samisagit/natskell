{-# LANGUAGE OverloadedStrings #-}

module Types.Unsub where

import           Data.ByteString       (ByteString)
import           Types.Msg             (SID)
import           Validators.Validators

data Unsub = Unsub
               { sid    :: SID
               , maxMsg :: Maybe Int
               }
  deriving (Eq, Show)

instance Validator Unsub where
  validate u = do
    validateSid u

validateSid :: Unsub -> Either ByteString ()
validateSid u
  | sid u == "" = Left "explicit empty sid"
  | otherwise = Right ()
