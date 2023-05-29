{-# LANGUAGE OverloadedStrings #-}

module Parsers.Parsers where

import           Control.Applicative
import qualified Control.Monad.Fail   as Fail
import           Data.Aeson
import           Data.ByteString
import qualified Data.ByteString.Lazy as BSL
import           Data.Word8
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

genericParse :: ByteString -> Either ParserErr (ParsedMessage, ByteString)
genericParse a = case result of
  Left s       -> Left s
  Right (p, bs) -> Right (p, bs)
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
  rest <- til _cr
  string "\r\n"

  case eitherDecode . BSL.fromStrict $ pack rest of
    Right a -> return (ParsedInfo a)
    Left e  -> Fail.fail $ "decode failed" ++ show e

msgParser =
  msgWithReplyAndPayloadparser
    <|> msgWithPayloadparser
    <|> msgWithReplyparser
    <|> msgMinParser
    <|> hmsgWithReplyAndPayloadParser
    <|> hmsgWithPayloadParser
    <|> hmsgWithReplyParser
    <|> hmsgMinParser

msgWithReplyAndPayloadparser :: Parser ParsedMessage
msgWithReplyAndPayloadparser = do
  string "MSG"
  ss
  subj <- subjectParser
  ss
  sid <- alphaNumerics
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
        (pack sid)
        (Just (pack reply))
        (Just (pack payload))
        Nothing
    )

msgWithPayloadparser :: Parser ParsedMessage
msgWithPayloadparser = do
  string "MSG"
  ss
  subj <- subjectParser
  ss
  sid <- alphaNumerics
  ss
  count <- integer
  string "\r\n"
  let countInt = toInt . pack $ count
  payload <- take' countInt ascii
  string "\r\n"
  return
    (ParsedMsg $ Msg
        (pack subj)
        (pack sid)
        Nothing
        (Just (pack payload))
        Nothing
    )

msgWithReplyparser :: Parser ParsedMessage
msgWithReplyparser = do
  string "MSG"
  ss
  subj <- subjectParser
  ss
  sid <- alphaNumerics
  ss
  reply <- subjectParser
  ss
  integer
  string "\r\n"
  return
    (ParsedMsg $ Msg
        (pack subj)
        (pack sid)
        (Just (pack reply))
        Nothing
        Nothing
    )

msgMinParser :: Parser ParsedMessage
msgMinParser = do
  string "MSG"
  ss
  subj <- subjectParser
  ss
  sid <- alphaNumerics
  ss
  integer
  string "\r\n"
  return
    (ParsedMsg $ Msg
        (pack subj)
        (pack sid)
        Nothing
        Nothing
        Nothing
    )

hmsgWithReplyAndPayloadParser = do
  string "HMSG"
  ss
  subj <- subjectParser
  ss
  sid <- alphaNumerics
  ss
  reply <- subjectParser
  ss
  hcount <- integer
  ss
  tcount <- integer
  string "\r\n"
  let hcountInt = toInt . pack $ hcount
  let pcountInt = (toInt . pack $ tcount) - (toInt . pack $ hcount)
  headers <- headersParser (hcountInt - 2) -- ignore the last line break
  string "\r\n"
  payload <- take' pcountInt ascii
  string "\r\n"
  return
    (ParsedMsg $ Msg
        (pack subj)
        (pack sid)
        (Just . pack $ reply)
        (Just . pack $ payload)
        (Just headers)
    )

hmsgWithPayloadParser = do
  string "HMSG"
  ss
  subj <- subjectParser
  ss
  sid <- alphaNumerics
  ss
  hcount <- integer
  ss
  tcount <- integer
  string "\r\n"
  let hcountInt = toInt . pack $ hcount
  let pcountInt = (toInt . pack $ tcount) - (toInt . pack $ hcount)
  headers <- headersParser (hcountInt - 2) -- ignore the last line break
  string "\r\n"
  payload <- take' pcountInt ascii
  string "\r\n"
  return
    (ParsedMsg $ Msg
        (pack subj)
        (pack sid)
        Nothing
        (Just . pack $ payload)
        (Just headers)
    )

hmsgWithReplyParser = do
  string "HMSG"
  ss
  subj <- subjectParser
  ss
  sid <- alphaNumerics
  ss
  reply <- subjectParser
  ss
  hcount <- integer
  ss
  integer
  string "\r\n"
  let hcountInt = toInt . pack $ hcount
  headers <- headersParser (hcountInt - 2) -- ignore the last line break
  string "\r\n"
  return
    (ParsedMsg $ Msg
        (pack subj)
        (pack sid)
        (Just . pack $ reply)
        Nothing
        (Just headers)
    )

hmsgMinParser = do
  string "HMSG"
  ss
  subj <- subjectParser
  ss
  sid <- alphaNumerics
  ss
  hcount <- integer
  ss
  integer
  string "\r\n"
  let hcountInt = toInt . pack $ hcount
  headers <- headersParser (hcountInt - 2) -- ignore the last line break
  string "\r\n"
  return
    (ParsedMsg $ Msg
        (pack subj)
        (pack sid)
        Nothing
        Nothing
        (Just headers)
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

