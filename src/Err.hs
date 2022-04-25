{-# LANGUAGE OverloadedStrings #-}

module Err where

import           Control.Applicative
import           Data.ByteString
import           Data.Word8
import           Parser

data Err = Err {
  reason :: ByteString,
  fatal  :: Bool
} deriving (Show, Eq)

errPrefixInvalidSubject :: ByteString
errPrefixInvalidSubject = "Invalid Subject"
errPrefixPerm :: ByteString
errPrefixPerm = "Permissions Violation"

parser :: Parser Err
parser = do
  string "-ERR"
  ss
  char _quotesingle
  reason <- reasonParser
  char _quotesingle
  string "\r\n"
  let packedReason = pack reason
  return (Err packedReason (isFatal packedReason))

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
  <|> subPermParser
  <|> pubPermParser

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
subPermParser = do
  string "Permissions Violation for Subscription to"
  ss
  asciis
pubPermParser = do
  string "Permissions Violation for Publish to"
  ss
  asciis

