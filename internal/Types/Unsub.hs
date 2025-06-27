{-# LANGUAGE OverloadedStrings #-}

module Types.Unsub where

import qualified Data.ByteString       as BS
import           Validators.Validators

data Unsub = Unsub
               { sid    :: BS.ByteString
               , maxMsg :: Maybe Int
               }
  deriving (Eq, Show)

instance Validator Unsub where
  validate u = do
    validateSid u

validateSid :: Unsub -> Either BS.ByteString ()
validateSid u
  | sid u == "" = Left "explicit empty sid"
  | otherwise = Right ()

