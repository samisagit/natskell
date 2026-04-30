{-# LANGUAGE OverloadedStrings #-}

module Parser.Attoparsec
  ( parserApi
  ) where

import           Control.Applicative              ((<|>))
import qualified Data.Aeson                       as Aeson
import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BSL
import           Data.Word                        (Word8)
import           Parser.API
    ( ParseStep (DropPrefix, Emit, NeedMore)
    , ParsedMessage (ParsedErr, ParsedInfo, ParsedMsg, ParsedOk, ParsedPing, ParsedPong)
    , ParserAPI (ParserAPI)
    )
import           Types.Err
    ( Err (ErrAuthTimeout, ErrAuthViolation, ErrErr, ErrInvalidProtocol, ErrInvalidSubject, ErrMaxConnsEx, ErrMaxControlLineEx, ErrMaxPayload, ErrPermViolation, ErrRoutePortConn, ErrSlowConsumer, ErrStaleConn, ErrTlsRequired, ErrUnknownOp)
    )
import           Types.Msg                        (Msg (Msg))
import           Types.Ok                         (Ok (Ok))
import           Types.Ping                       (Ping (Ping))
import           Types.Pong                       (Pong (Pong))

parserApi :: ParserAPI ParsedMessage
parserApi = ParserAPI parseStep

parseStep :: BS.ByteString -> ParseStep ParsedMessage
parseStep bytes =
  case A.parse parsedMessageParser bytes of
    A.Done rest parsedMessage ->
      Emit parsedMessage rest
    A.Partial _ ->
      NeedMore
    A.Fail rest _ reason ->
      DropPrefix (dropBytes bytes rest) reason

parsedMessageParser :: A.Parser ParsedMessage
parsedMessageParser = do
  prefix <- A.peekWord8'
  case prefix of
    43 ->
      okParser
    45 ->
      errParser
    72 ->
      hmsgParser
    73 ->
      infoParser
    77 ->
      msgParser
    80 ->
      pingOrPongParser
    _ ->
      fail ("unknown protocol prefix: " ++ show prefix)

okParser :: A.Parser ParsedMessage
okParser = do
  _ <- A.string "+OK\r\n"
  pure (ParsedOk Ok)

errParser :: A.Parser ParsedMessage
errParser = do
  _ <- A.string "-ERR"
  skipHorizontalSpace1
  _ <- A.word8 singleQuote
  reason <- A.takeTill (== singleQuote)
  _ <- A.word8 singleQuote
  _ <- A.string "\r\n"
  case classifyErr reason of
    Left parseReason ->
      fail parseReason
    Right err ->
      pure (ParsedErr err)

infoParser :: A.Parser ParsedMessage
infoParser = do
  _ <- A.string "INFO"
  skipHorizontalSpace1
  rawInfo <- lineParser
  case Aeson.eitherDecode (BSL.fromStrict rawInfo) of
    Left parseReason ->
      fail ("invalid INFO frame: " ++ parseReason)
    Right info ->
      pure (ParsedInfo info)

msgParser :: A.Parser ParsedMessage
msgParser = do
  _ <- A.string "MSG"
  skipHorizontalSpace1
  controlLine <- lineParser
  case B8.words controlLine of
    [subjectName, sidValue, payloadSizeBytes] ->
      parseMsgPayload subjectName sidValue Nothing payloadSizeBytes
    [subjectName, sidValue, replySubject, payloadSizeBytes] ->
      parseMsgPayload subjectName sidValue (Just replySubject) payloadSizeBytes
    _ ->
      fail ("invalid MSG control line: " ++ B8.unpack controlLine)

hmsgParser :: A.Parser ParsedMessage
hmsgParser = do
  _ <- A.string "HMSG"
  skipHorizontalSpace1
  controlLine <- lineParser
  case B8.words controlLine of
    [subjectName, sidValue, headerSizeBytes, totalSizeBytes] ->
      parseHMsgPayload subjectName sidValue Nothing headerSizeBytes totalSizeBytes
    [subjectName, sidValue, replySubject, headerSizeBytes, totalSizeBytes] ->
      parseHMsgPayload subjectName sidValue (Just replySubject) headerSizeBytes totalSizeBytes
    _ ->
      fail ("invalid HMSG control line: " ++ B8.unpack controlLine)

pingOrPongParser :: A.Parser ParsedMessage
pingOrPongParser =
  pingParser <|> pongParser

pingParser :: A.Parser ParsedMessage
pingParser = do
  _ <- A.string "PING\r\n"
  pure (ParsedPing Ping)

pongParser :: A.Parser ParsedMessage
pongParser = do
  _ <- A.string "PONG\r\n"
  pure (ParsedPong Pong)

parseMsgPayload :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString -> BS.ByteString -> A.Parser ParsedMessage
parseMsgPayload subjectName sidValue replySubject payloadSizeBytes =
  case integerBytes "MSG payload size" payloadSizeBytes of
    Left parseReason ->
      fail parseReason
    Right payloadSize -> do
      payloadBytes <- A.take payloadSize
      _ <- A.string "\r\n"
      pure
        ( ParsedMsg
            ( Msg
                subjectName
                sidValue
                replySubject
                (nonEmpty payloadBytes)
                Nothing
            )
        )

parseHMsgPayload :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString -> BS.ByteString -> BS.ByteString -> A.Parser ParsedMessage
parseHMsgPayload subjectName sidValue replySubject headerSizeBytes totalSizeBytes =
  case (integerBytes "HMSG header size" headerSizeBytes, integerBytes "HMSG total size" totalSizeBytes) of
    (Left parseReason, _) ->
      fail parseReason
    (_, Left parseReason) ->
      fail parseReason
    (Right headerSize, Right totalSize)
      | totalSize < headerSize ->
          fail
            ( "invalid HMSG sizes: total size "
                ++ show totalSize
                ++ " is smaller than header size "
                ++ show headerSize
            )
      | otherwise -> do
          headerBytes <- A.take headerSize
          payloadBytes <- A.take (totalSize - headerSize)
          _ <- A.string "\r\n"
          case A.parseOnly (headerBlockParser <* A.endOfInput) headerBytes of
            Left parseReason ->
              fail ("invalid HMSG headers: " ++ parseReason)
            Right parsedHeaders ->
              pure
                ( ParsedMsg
                    ( Msg
                        subjectName
                        sidValue
                        replySubject
                        (Just payloadBytes)
                        (Just parsedHeaders)
                    )
                )

headerBlockParser :: A.Parser [(BS.ByteString, BS.ByteString)]
headerBlockParser = do
  _ <- A.string "NATS/"
  _ <- lineParser
  headerPairsParser []

headerPairsParser :: [(BS.ByteString, BS.ByteString)] -> A.Parser [(BS.ByteString, BS.ByteString)]
headerPairsParser reversedPairs = do
  nextByte <- A.peekWord8'
  if nextByte == carriageReturn
    then do
      _ <- A.string "\r\n"
      pure (reverse reversedPairs)
    else do
      headerKey <- A.takeTill (== colon)
      _ <- A.word8 colon
      headerValue <- lineParser
      headerPairsParser ((stripHorizontalSpace headerKey, stripHorizontalSpace headerValue) : reversedPairs)

classifyErr :: BS.ByteString -> Either String Err
classifyErr reason
  | reason == "Unknown Protocol Operation" =
      Right (ErrUnknownOp reason)
  | reason == "Attempted To Connect To Route Port" =
      Right (ErrRoutePortConn reason)
  | reason == "Authorization Violation" =
      Right (ErrAuthViolation reason)
  | reason == "Authorization Timeout" =
      Right (ErrAuthTimeout reason)
  | reason == "Invalid Client Protocol" =
      Right (ErrInvalidProtocol reason)
  | reason == "Maximum Control Line Exceeded" =
      Right (ErrMaxControlLineEx reason)
  | reason == "Parser Error" =
      Right (ErrErr reason)
  | reason == "Secure Connection - TLS Required" =
      Right (ErrTlsRequired reason)
  | reason == "Stale Connection" =
      Right (ErrStaleConn reason)
  | reason == "Maximum Connections Exceeded" =
      Right (ErrMaxConnsEx reason)
  | reason == "Slow Consumer" =
      Right (ErrSlowConsumer reason)
  | reason == "Maximum Payload Violation" =
      Right (ErrMaxPayload reason)
  | reason == "Invalid Subject" =
      Right (ErrInvalidSubject reason)
  | "Permissions Violation" `BS.isPrefixOf` reason =
      Right (ErrPermViolation reason)
  | otherwise =
      Left ("unknown -ERR reason: " ++ B8.unpack reason)

lineParser :: A.Parser BS.ByteString
lineParser = do
  lineBytes <- A.takeTill (== carriageReturn)
  _ <- A.string "\r\n"
  pure lineBytes

skipHorizontalSpace1 :: A.Parser ()
skipHorizontalSpace1 = do
  _ <- A.satisfy isHorizontalSpaceByte
  A.skipWhile isHorizontalSpaceByte

integerBytes :: String -> BS.ByteString -> Either String Int
integerBytes label digits =
  case A.parseOnly (AC.decimal <* A.endOfInput) digits of
    Left _ ->
      Left (label ++ " is not an integer: " ++ B8.unpack digits)
    Right value ->
      Right value

dropBytes :: BS.ByteString -> BS.ByteString -> Int
dropBytes input remaining =
  min (BS.length input) (consumedBytes + 1)
  where
    consumedBytes = BS.length input - BS.length remaining

nonEmpty :: BS.ByteString -> Maybe BS.ByteString
nonEmpty bytes
  | BS.null bytes =
      Nothing
  | otherwise =
      Just bytes

stripHorizontalSpace :: BS.ByteString -> BS.ByteString
stripHorizontalSpace =
  BS.dropWhile isHorizontalSpaceByte . dropWhileEnd isHorizontalSpaceByte

dropWhileEnd :: (Word8 -> Bool) -> BS.ByteString -> BS.ByteString
dropWhileEnd predicate =
  BS.reverse . BS.dropWhile predicate . BS.reverse

isHorizontalSpaceByte :: Word8 -> Bool
isHorizontalSpaceByte word8Value =
  word8Value == space || word8Value == horizontalTab

carriageReturn :: Word8
carriageReturn = 13

singleQuote :: Word8
singleQuote = 39

colon :: Word8
colon = 58

space :: Word8
space = 32

horizontalTab :: Word8
horizontalTab = 9
