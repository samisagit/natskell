{-# LANGUAGE OverloadedStrings #-}

module Types.Sub where

import           Data.ByteString       (ByteString)
import           Types.Msg             (SID, Subject)
import           Validators.Validators

data Sub = Sub
             { subject    :: Subject
             , queueGroup :: Maybe Subject
             , sid        :: SID
             }
  deriving (Eq, Show)

instance Validator Sub where
  validate s = do
    validateSubject s
    validateQueueGroup s
    validateSid s

validateSubject :: Sub -> Either ByteString ()
validateSubject s
  | subject s == "" = Left "explicit empty subject"
  | otherwise = Right ()

validateQueueGroup :: Sub -> Either ByteString ()
validateQueueGroup s
  | queueGroup s == Just "" = Left "explicit empty queue group"
  | otherwise = Right ()

validateSid :: Sub -> Either ByteString ()
validateSid s
  | sid s == "" = Left "explicit empty sid"
  | otherwise = Right ()
