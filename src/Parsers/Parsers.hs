{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import           Control.Applicative
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

data Generic = ParsedPing Ping | ParsedPong Pong -- see if just Ping | Pong works

genericParse :: ByteString -> IO ()
genericParse a = case (runParser (oparser <|> iparser) a) of
  Just (p, _) -> case p of
    ParsedPing a -> print "parsed a ping"
    ParsedPong a -> print "parsed a pong"
  Nothing     -> return ()

errParser :: Parser Err
errParser = do
  string "-ERR"
  ss
  char _quotesingle
  reason <- reasonParser
  char _quotesingle
  string "\r\n"
  let packedReason = pack reason
  return (Err packedReason (isFatal packedReason))

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

infoParser :: Parser (Maybe Info)
infoParser = do
  string "INFO"
  ss
  rest <- asciis
  let packed = pack rest
  return (decode . BSL.fromStrict $ packed)

msgParser =
  msgWithReplyAndPayloadparser
    <|> msgWithPayloadparser
    <|> msgWithReplyparser
    <|> msgMinParser

msgWithReplyAndPayloadparser :: Parser Data
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
    ( Data
        (pack subj)
        (toInt . pack $ sid)
        (Just (pack reply))
        countInt
        (Just (pack payload))
    )

msgWithPayloadparser :: Parser Data
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
    ( Data
        (pack subj)
        (toInt . pack $ sid)
        Nothing
        countInt
        (Just (pack payload))
    )

msgWithReplyparser :: Parser Data
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
    ( Data
        (pack subj)
        (toInt . pack $ sid)
        (Just (pack reply))
        (toInt . pack $ count)
        Nothing
    )

msgMinParser :: Parser Data
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
    ( Data
        (pack subj)
        (toInt . pack $ sid)
        Nothing
        (toInt . pack $ count)
        Nothing
    )

okParser :: Parser Ok
okParser = do
  string "+OK\r\n"
  return Ok


pingParser :: Parser Ping
pingParser = do
  string "PING\r\n"
  return Ping

pongParser :: Parser Pong
pongParser = do
  string "PONG\r\n"
  return Pong

