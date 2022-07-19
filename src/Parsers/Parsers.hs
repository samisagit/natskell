{-# LANGUAGE OverloadedStrings #-}

module Parsers.Parsers where

import           Control.Applicative
import qualified Control.Monad.Fail   as Fail
import           Data.Aeson
import           Data.ByteString
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Data.Word8
import           Debug.Trace
import           Lib.Parser
import           Types.Err
import           Types.Info
import           Types.Msg
import           Types.Ok
import           Types.Ping
import           Types.Pong

-- TODO do we need the wrapping type
data ParsedMessage = ParsedPing Ping
  | ParsedPong Pong
  | ParsedOk Ok
  | ParsedErr Err
  | ParsedInfo Info
  | ParsedMsg Msg
  deriving (Show, Eq)

genericParse :: ByteString -> Maybe ParsedMessage
genericParse a = case result of
  Just (p, _) -> return p
  Nothing     -> Nothing
  where
    result = runParser (
      pongParser
      <|> pingParser
      <|> msgParser
      <|> infoParser
      <|> errParser
      <|> okParser
      ) a

errParser :: Parser ParsedMessage
errParser = do
  string "-ERR"
  ss
  char _quotesingle
  reason <- reasonParser
  char _quotesingle
  string "\r\n"
  let packedReason = pack reason
  return (ParsedErr $ Err packedReason (isFatal packedReason))

errPrefixInvalidSubject = "Invalid Subject"
errPrefixPerm = "Permissions Violation"

reasonParser :: Parser [Word8]
reasonParser = unknownOpParser
  <|> routePortConnParser
  <|> authViolationParser
  <|> authTimeoutParser
  <|> invalidProtocolParser
  <|> maxControlLineExParser
  <|> parserErrParser
  <|> tlsRequiredParser
  <|> staleConnParser
  <|> maxConnsExParser
  <|> slowConsumerParser
  <|> maxPayloadParser
  <|> invalidSubjParser
  <|> permViolationParser

isFatal :: ByteString -> Bool
isFatal err = (errPrefixInvalidSubject /= err) && not (errPrefixPerm `isPrefixOf` err)

unknownOpParser = string "Unknown Protocol Operation"
routePortConnParser = string "Attempted To Connect To Route Port"
authViolationParser = string "Authorization Violation"
authTimeoutParser = string "Authorization Timeout"
invalidProtocolParser = string "Invalid Client Protocol"
maxControlLineExParser = string "Maximum Control Line Exceeded"
parserErrParser = string "Parser Error"
tlsRequiredParser = string "Secure Connection - TLS Required"
staleConnParser = string "Stale Connection"
maxConnsExParser = string "Maximum Connections Exceeded"
slowConsumerParser = string "Slow Consumer"
maxPayloadParser = string "Maximum Payload Violation"
invalidSubjParser = string "Invalid Subject"

-- permission parsers are tricky because the violation reason could be complete nonsense
-- so we just consume until the single quote delimiter
permViolationParser :: Parser [Word8]
permViolationParser = do
  pre <- string "Permissions Violation"
  post <- til _quotesingle
  return (pre ++ post)

infoParser :: Parser ParsedMessage
infoParser = do
  string "INFO"
  ss
  rest <- asciis
  case eitherDecode . BSL.fromStrict $ pack rest of
    Right a -> return (ParsedInfo a)
    Left e  -> Fail.fail "decode failed"

msgParser =
  msgWithReplyAndPayloadparser
    <|> msgWithPayloadparser
    <|> msgWithReplyparser
    <|> msgMinParser

msgWithReplyAndPayloadparser :: Parser ParsedMessage
msgWithReplyAndPayloadparser = do
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
  let countInt = toInt . pack $ count
  payload <- take' countInt ascii
  string "\r\n"
  return
    (ParsedMsg $ Msg
        (pack subj)
        (toInt . pack $ sid)
        (Just (pack reply))
        countInt
        (Just (pack payload))
    )

msgWithPayloadparser :: Parser ParsedMessage
msgWithPayloadparser = do
  string "MSG"
  ss
  subj <- subjectParser
  ss
  sid <- integer
  ss
  count <- integer
  string "\r\n"
  let countInt = toInt . pack $ count
  payload <- take' countInt ascii
  string "\r\n"
  return
    (ParsedMsg $ Msg
        (pack subj)
        (toInt . pack $ sid)
        Nothing
        countInt
        (Just (pack payload))
    )

msgWithReplyparser :: Parser ParsedMessage
msgWithReplyparser = do
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
    (ParsedMsg $ Msg
        (pack subj)
        (toInt . pack $ sid)
        (Just (pack reply))
        (toInt . pack $ count)
        Nothing
    )

msgMinParser :: Parser ParsedMessage
msgMinParser = do
  string "MSG"
  ss
  subj <- subjectParser
  ss
  sid <- integer
  ss
  count <- integer
  string "\r\n"
  return
    (ParsedMsg $ Msg
        (pack subj)
        (toInt . pack $ sid)
        Nothing
        (toInt . pack $ count)
        Nothing
    )

okParser :: Parser ParsedMessage
okParser = do
  string "+OK\r\n"
  return (ParsedOk Ok)


pingParser :: Parser ParsedMessage
pingParser = do
  string "PING\r\n"
  return (ParsedPing Ping)

pongParser :: Parser ParsedMessage
pongParser = do
  string "PONG\r\n"
  return (ParsedPong Pong)

