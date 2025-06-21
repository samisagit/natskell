{-# LANGUAGE OverloadedStrings #-}

module Types.Sub where

import qualified Data.ByteString       as BS
import           Validators.Validators

data Sub = Sub
             { subject    :: BS.ByteString
             , queueGroup :: Maybe BS.ByteString
             , sid        :: BS.ByteString
             }
  deriving (Eq, Show)

instance Validator Sub where
  validate s = do
    validateSubject s
    validateQueueGroup s
    validateSid s

validateSubject :: Sub -> Either BS.ByteString ()
validateSubject s
  | subject s == "" = Left "explicit empty subject"
  | otherwise = Right ()

validateQueueGroup :: Sub -> Either BS.ByteString ()
validateQueueGroup s
  | queueGroup s == Just "" = Left "explicit empty queue group"
  | otherwise = Right ()

validateSid :: Sub -> Either BS.ByteString ()
validateSid s
  | sid s == "" = Left "explicit empty sid"
  | otherwise = Right ()

