{-# LANGUAGE OverloadedStrings #-}

module Types.Pub where

import           Data.ByteString       hiding (foldr, map)
import           Data.Maybe
import           Prelude               hiding (concat, length, null)
import           Validators.Validators

data Pub = Pub
             { subject :: ByteString
             , replyTo :: Maybe ByteString
             , headers :: Maybe [(ByteString, ByteString)]
             , payload :: Maybe ByteString
             }
  deriving (Eq, Show)

instance Validator Pub where
  validate p = do
    validateSubject p
    validateReplyTo p
    validatePayload p
    validateHeaders p

validateSubject :: Pub -> Either ByteString ()
validateSubject p
  | subject p == "" = Left "explicit empty subject"
  | otherwise = Right ()

validateReplyTo :: Pub -> Either ByteString ()
validateReplyTo p
  | replyTo p == Just "" = Left "explicit empty replyTo"
  | otherwise = Right ()

validatePayload :: Pub -> Either ByteString ()
validatePayload p
  | payload p == Just "" = Left "explicit empty payload"
  | otherwise = Right ()

validateHeaders :: Pub -> Either ByteString ()
validateHeaders p
  | isNothing (headers p) = Right ()
  | headers p == Just [] = Left "explicit empty headers"
  | Prelude.any (\(k, _) -> k == "") (fromJust (headers p)) = Left "explicit empty header key"
  | Prelude.any (\(_, v) -> v == "") (fromJust (headers p)) = Left "explicit empty header value"
  | otherwise = Right ()

