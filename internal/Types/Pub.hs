{-# LANGUAGE OverloadedStrings #-}

module Types.Pub where

import           Data.ByteString
import           Data.Maybe
import           Validators.Validators

data Pub = Pub
  { subject :: ByteString,
    replyTo :: Maybe ByteString,
    headers :: Maybe [(ByteString, ByteString)],
    payload :: Maybe ByteString
  }
  deriving (Eq, Show)

instance Validator Pub where
  validate p
    | Types.Pub.subject p == "" = Just "explicit empty subject"
    | Types.Pub.replyTo p == Just "" = Just "explicit empty replyTo"
    | Types.Pub.payload p == Just "" = Just "explicit empty payload"
    | isJust (validateHeaders p) = validateHeaders p
    | otherwise = Nothing

validateHeaders :: Pub -> Maybe ByteString
validateHeaders p
  | isNothing (Types.Pub.headers p) = Nothing
  | Types.Pub.headers p == Just [] = Just "explicit empty headers"
  | Prelude.any (\(k, _) -> k == "") (fromJust (Types.Pub.headers p)) = Just "explicit empty header key"
  | Prelude.any (\(_, v) -> v == "") (fromJust (Types.Pub.headers p)) = Just "explicit empty header value"
  | otherwise = Nothing

