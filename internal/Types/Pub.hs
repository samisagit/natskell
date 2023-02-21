{-# LANGUAGE OverloadedStrings #-}

module Types.Pub where

import           Data.ByteString       hiding (foldr, map)
import           Data.Maybe
import           Prelude               hiding (concat, length, null)
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
    | subject p == "" = Just "explicit empty subject"
    | replyTo p == Just "" = Just "explicit empty replyTo"
    | payload p == Just "" = Just "explicit empty payload"
    | isJust (validateHeaders p) = validateHeaders p
    | otherwise = Nothing

validateHeaders :: Pub -> Maybe ByteString
validateHeaders p
  | isNothing (headers p) = Nothing
  | headers p == Just [] = Just "explicit empty headers"
  | Prelude.any (\(k, _) -> k == "") (fromJust (headers p)) = Just "explicit empty header key"
  | Prelude.any (\(_, v) -> v == "") (fromJust (headers p)) = Just "explicit empty header value"
  | otherwise = Nothing

