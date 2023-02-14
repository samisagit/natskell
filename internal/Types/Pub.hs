{-# LANGUAGE OverloadedStrings #-}

module Types.Pub where

import           Data.ByteString
import           Validators.Validators

data Pub = Pub
  { subject :: ByteString,
    replyTo :: Maybe ByteString,
    payload :: Maybe ByteString
  }
  deriving (Eq, Show)

instance Validator Pub where
  validate p
    | Types.Pub.subject p == "" = Just "explicit empty subject"
    | Types.Pub.replyTo p == Just "" = Just "explicit empty replyTo"
    | Types.Pub.payload p == Just "" = Just "explicit empty payload"
    | otherwise = Nothing
