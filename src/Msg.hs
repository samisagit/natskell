{-# LANGUAGE OverloadedStrings #-}

module Msg (Data, subject, sid, replyTo, byteCount, payload, parser) where

import           Control.Applicative
import qualified Data.ByteString     as BS
import qualified Data.Word8          as W8
import           Parser

data Data = Data
  { subject   :: BS.ByteString,
    sid       :: Int,
    replyTo   :: Maybe BS.ByteString,
    byteCount :: Int,
    payload   :: Maybe BS.ByteString
  }

parser =
  withReplyAndPayloadparser
    <|> withPayloadparser
    <|> withReplyparser
    <|> minParser

withReplyAndPayloadparser :: Parser Data
withReplyAndPayloadparser = do
  string "MSG"
  ss
  subj <- subjectParser
  ss
  sid <- integer
  ss
  reply <- subjectParser
  ss
  count <- integer
  string "\r\n"
  let countInt = toInt . BS.pack $ count
  payload <- take' countInt ascii
  string "\r\n"
  return
    ( Data
        (BS.pack subj)
        (toInt . BS.pack $ sid)
        (Just (BS.pack reply))
        countInt
        (Just (BS.pack payload))
    )

withPayloadparser :: Parser Data
withPayloadparser = do
  string "MSG"
  ss
  subj <- subjectParser
  ss
  sid <- integer
  ss
  count <- integer
  string "\r\n"
  let countInt = toInt . BS.pack $ count
  payload <- take' countInt ascii
  string "\r\n"
  return
    ( Data
        (BS.pack subj)
        (toInt . BS.pack $ sid)
        Nothing
        countInt
        (Just (BS.pack payload))
    )

withReplyparser :: Parser Data
withReplyparser = do
  string "MSG"
  ss
  subj <- subjectParser
  ss
  sid <- integer
  ss
  reply <- subjectParser
  ss
  count <- integer
  string "\r\n"
  return
    ( Data
        (BS.pack subj)
        (toInt . BS.pack $ sid)
        (Just (BS.pack reply))
        (toInt . BS.pack $ count)
        Nothing
    )

minParser :: Parser Data
minParser = do
  string "MSG"
  ss
  subj <- subjectParser
  ss
  sid <- integer
  ss
  count <- integer
  string "\r\n"
  return
    ( Data
        (BS.pack subj)
        (toInt . BS.pack $ sid)
        Nothing
        (toInt . BS.pack $ count)
        Nothing
    )
