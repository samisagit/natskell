{-# LANGUAGE OverloadedStrings #-}

module Parser.Nats where

import           Control.Applicative
import qualified Control.Monad.Fail   as Fail
import           Data.Aeson
import           Data.ByteString
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Word8
import           Parser.API
    ( ParseStep (DropPrefix, Emit, NeedMore)
    , ParsedMessage (..)
    , ParserAPI (ParserAPI)
    )
import           Parser.Combinators
import           Types.Err
import           Types.Msg
import           Types.Ok
import           Types.Ping
import           Types.Pong

parserApi :: ParserAPI ParsedMessage
parserApi = ParserAPI parseStep

parseStep :: ByteString -> ParseStep ParsedMessage
parseStep bytes =
  case genericParse bytes of
    Right (message, rest) ->
      Emit message rest
    Left err ->
      case solveErr err (BS.length bytes) of
        SuggestPull ->
          NeedMore
        SuggestDrop n reason ->
          DropPrefix n reason

genericParse :: ByteString -> Either ParserErr (ParsedMessage, ByteString)
genericParse a = case result of
  Left s        -> Left s
  Right (p, bs) -> Right (p, bs)
  where
    result = runParser (
      msgParser
      <|>pongParser
      <|> pingParser
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
  return reason

errPrefixInvalidSubject = "Invalid Subject"
errPrefixPerm = "Permissions Violation"

reasonParser :: Parser ParsedMessage
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
  >> return (ParsedErr (ErrUnknownOp "Unknown Protocol Operation"))
routePortConnParser = string "Attempted To Connect To Route Port"
  >> return (ParsedErr (ErrRoutePortConn "Attempted To Connect To Route Port"))
authViolationParser = string "Authorization Violation"
  >> return (ParsedErr (ErrAuthViolation "Authorization Violation"))
authTimeoutParser = string "Authorization Timeout"
  >> return (ParsedErr (ErrAuthTimeout "Authorization Timeout"))
invalidProtocolParser = string "Invalid Client Protocol"
  >> return (ParsedErr (ErrInvalidProtocol "Invalid Client Protocol"))
maxControlLineExParser = string "Maximum Control Line Exceeded"
  >> return (ParsedErr (ErrMaxControlLineEx "Maximum Control Line Exceeded"))
parserErrParser = string "Parser Error"
  >> return (ParsedErr (ErrErr "Parser Error"))
tlsRequiredParser = string "Secure Connection - TLS Required"
  >> return (ParsedErr (ErrTlsRequired "Secure Connection - TLS Required"))
staleConnParser = string "Stale Connection"
  >> return (ParsedErr (ErrStaleConn "Stale Connection"))
maxConnsExParser = string "Maximum Connections Exceeded"
  >> return (ParsedErr (ErrMaxConnsEx "Maximum Connections Exceeded"))
slowConsumerParser = string "Slow Consumer"
  >> return (ParsedErr (ErrSlowConsumer "Slow Consumer"))
maxPayloadParser = string "Maximum Payload Violation"
  >> return (ParsedErr (ErrMaxPayload "Maximum Payload Violation"))
invalidSubjParser = string "Invalid Subject"
  >> return (ParsedErr (ErrInvalidSubject errPrefixInvalidSubject))

-- permission parsers are tricky because the violation reason could be complete nonsense
-- so we just consume until the single quote delimiter
permViolationParser :: Parser ParsedMessage
permViolationParser = do
  pre <- string "Permissions Violation"
  post <- til _quotesingle
  let reason = pre <> post
  return (ParsedErr (ErrPermViolation (pack reason)))

infoParser :: Parser ParsedMessage
infoParser = do
  string "INFO"
  spaces1
  rest <- takeTill1 _cr
  string "\r\n"

  case eitherDecode . BSL.fromStrict $ rest of
    Right a -> return (ParsedInfo a)
    Left e  -> Fail.fail $ "decode failed" ++ show e

msgParser =
  msgWithReplyAndPayloadparser
    <|> msgWithPayloadparser
    <|> msgWithReplyparser
    <|> hmsgWithReplyAndPayloadParser
    <|> hmsgWithPayloadParser
    <|> hmsgWithReplyParser

msgWithReplyAndPayloadparser :: Parser ParsedMessage
msgWithReplyAndPayloadparser = do
  string "MSG"
  spaces1
  subj <- subjectParserBS
  sid <- alphaNumericsBS
  spaces1
  reply <- subjectParserBS
  count <- integerBS
  let countInt = toIntBS count
  string "\r\n"
  payload <- takeBytes countInt
  string "\r\n"
  return
    (ParsedMsg $ Msg
        subj
        sid
        (Just reply)
        (if BS.null payload then Nothing else Just payload)
        Nothing
    )

msgWithPayloadparser :: Parser ParsedMessage
msgWithPayloadparser = do
  string "MSG"
  spaces1
  subj <- subjectParserBS
  sid <- alphaNumericsBS
  spaces1
  count <- integerBS
  let countInt = toIntBS count
  string "\r\n"
  payload <- takeBytes countInt
  string "\r\n"
  return
    (ParsedMsg $ Msg
        subj
        sid
        Nothing
        (if BS.null payload then Nothing else Just payload)
        Nothing
    )

msgWithReplyparser :: Parser ParsedMessage
msgWithReplyparser = do
  string "MSG"
  spaces1
  subj <- subjectParserBS
  sid <- alphaNumericsBS
  spaces1
  reply <- subjectParserBS
  _ <- integerBS
  string "\r\n"
  string "\r\n"
  return
    (ParsedMsg $ Msg
        subj
        sid
        (Just reply)
        Nothing
        Nothing
    )

hmsgWithReplyAndPayloadParser = do
  string "HMSG"
  spaces1
  subj <- subjectParserBS
  sid <- alphaNumericsBS
  spaces1
  reply <- subjectParserBS
  hcount <- integerBS
  spaces1
  tcount <- integerBS
  string "\r\n"
  let hcountInt = toIntBS hcount
  let pcountInt = toIntBS tcount - hcountInt
  headers <- headersParser (hcountInt - 2) -- ignore the last line break
  string "\r\n"
  payload <- takeBytes pcountInt
  string "\r\n"
  return
    (ParsedMsg $ Msg
        subj
        sid
        (Just reply)
        (Just payload)
        (Just headers)
    )

hmsgWithPayloadParser = do
  string "HMSG"
  spaces1
  subj <- subjectParserBS
  sid <- alphaNumericsBS
  spaces1
  hcount <- integerBS
  spaces1
  tcount <- integerBS
  string "\r\n"
  let hcountInt = toIntBS hcount
  let pcountInt = toIntBS tcount - hcountInt
  headers <- headersParser (hcountInt - 2) -- ignore the last line break
  string "\r\n"
  payload <- takeBytes pcountInt
  string "\r\n"
  return
    (ParsedMsg $ Msg
        subj
        sid
        Nothing
        (Just payload)
        (Just headers)
    )

hmsgWithReplyParser = do
  string "HMSG"
  spaces1
  subj <- subjectParserBS
  sid <- alphaNumericsBS
  spaces1
  reply <- subjectParserBS
  hcount <- integerBS
  spaces1
  _ <- integerBS
  string "\r\n"
  let hcountInt = toIntBS hcount
  headers <- headersParser (hcountInt - 2) -- ignore the last line break
  string "\r\n"
  string "\r\n"
  return
    (ParsedMsg $ Msg
        subj
        sid
        (Just reply)
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
