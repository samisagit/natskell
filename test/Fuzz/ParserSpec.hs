{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Data.Bifunctor            (bimap)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as B8
import qualified Data.ByteString.Lazy      as LBS
import           Data.Maybe                (fromMaybe)
import           Data.Word                 (Word8)
import           Parser.API
    ( ParseStep (DropPrefix, Emit, NeedMore)
    , ParsedMessage (ParsedErr, ParsedInfo, ParsedMsg, ParsedOk, ParsedPing, ParsedPong)
    , parse
    )
import           Parser.Attoparsec         (parserApi)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (modifyMaxSuccess)
import           Test.QuickCheck
import           Transformers.Transformers (Transformer (transform))
import           Types.Err
    ( Err (ErrAuthTimeout, ErrAuthViolation, ErrErr, ErrInvalidProtocol, ErrInvalidSubject, ErrMaxConnsEx, ErrMaxControlLineEx, ErrMaxPayload, ErrPermViolation, ErrRoutePortConn, ErrSlowConsumer, ErrStaleConn, ErrTlsRequired, ErrUnknownOp)
    )
import           Types.Info
    ( Info (Info)
    , auth_required
    , client_id
    , connect_urls
    , go
    , headers
    , host
    , ldm
    , max_payload
    , nonce
    , port
    , proto
    , server_id
    , tls_required
    , version
    )
import qualified Types.Msg                 as Msg
import           Types.Msg                 (Msg (Msg))
import           Types.Ok                  (Ok (Ok))
import           Types.Ping                (Ping (Ping))
import           Types.Pong                (Pong (Pong))

spec :: Spec
spec = do
  describe "parser fuzz" $ do
    modifyMaxSuccess (const 5000) .
      it "parses generated valid frames" . property $
        propParsesValidFrames
    modifyMaxSuccess (const 5000) .
      it "requests more input for proper prefixes of valid frames" . property $
        propNeedsMoreForProperPrefixes
    modifyMaxSuccess (const 5000) .
      it "leaves the remaining bytes untouched after one parsed frame" . property $
        propLeavesRemainingBytes
    modifyMaxSuccess (const 5000) .
      it "recovers from generated noisy prefixes" . property $
        propRecoversFromNoisyPrefixes

data ValidFrame = ValidFrame
                    { frameBytes  :: BS.ByteString
                    , parsedValue :: ParsedMessage
                    }
  deriving (Eq, Show)

instance Arbitrary ValidFrame where
  arbitrary =
    oneof
      [ pure (ValidFrame (LBS.toStrict (transform Ping)) (ParsedPing Ping))
      , pure (ValidFrame (LBS.toStrict (transform Pong)) (ParsedPong Pong))
      , pure (ValidFrame "+OK\r\n" (ParsedOk Ok))
      , genErrFrame
      , genInfoFrame
      , genMsgFrame
      ]

propParsesValidFrames :: ValidFrame -> Property
propParsesValidFrames validFrame =
  parse parserApi (frameBytes validFrame)
    === Emit (parsedValue validFrame) ""

propNeedsMoreForProperPrefixes :: ValidFrame -> Property
propNeedsMoreForProperPrefixes validFrame =
  not (null candidatePrefixes)
    ==>
      forAll (elements candidatePrefixes) (\prefixBytes -> parse parserApi prefixBytes === NeedMore)
  where
    bytes = frameBytes validFrame
    candidatePrefixes = map (`BS.take` bytes) [1 .. BS.length bytes - 1]

propLeavesRemainingBytes :: ValidFrame -> ValidFrame -> Property
propLeavesRemainingBytes firstFrame secondFrame =
  parse parserApi (frameBytes firstFrame <> frameBytes secondFrame)
    === Emit (parsedValue firstFrame) (frameBytes secondFrame)

propRecoversFromNoisyPrefixes :: ValidFrame -> Property
propRecoversFromNoisyPrefixes validFrame =
  forAll genNoisePrefix $ \noisePrefix ->
    recoverParse (noisePrefix <> frameBytes validFrame)
      === Emit (parsedValue validFrame) ""

recoverParse :: BS.ByteString -> ParseStep ParsedMessage
recoverParse bytes =
  case parse parserApi bytes of
    DropPrefix dropped _
      | dropped <= 0 ->
          error "parser requested a non-positive prefix drop"
    DropPrefix dropped _ ->
      recoverParse (BS.drop dropped bytes)
    result ->
      result

genNoisePrefix :: Gen BS.ByteString
genNoisePrefix =
  fmap BS.pack (listOf1 (elements [33, 48, 63, 90, 95, 122]))

genErrFrame :: Gen ValidFrame
genErrFrame =
  oneof
    [ pure (errFrame "Unknown Protocol Operation" (ErrUnknownOp "Unknown Protocol Operation"))
    , pure (errFrame "Attempted To Connect To Route Port" (ErrRoutePortConn "Attempted To Connect To Route Port"))
    , pure (errFrame "Authorization Violation" (ErrAuthViolation "Authorization Violation"))
    , pure (errFrame "Authorization Timeout" (ErrAuthTimeout "Authorization Timeout"))
    , pure (errFrame "Invalid Client Protocol" (ErrInvalidProtocol "Invalid Client Protocol"))
    , pure (errFrame "Maximum Control Line Exceeded" (ErrMaxControlLineEx "Maximum Control Line Exceeded"))
    , pure (errFrame "Parser Error" (ErrErr "Parser Error"))
    , pure (errFrame "Secure Connection - TLS Required" (ErrTlsRequired "Secure Connection - TLS Required"))
    , pure (errFrame "Stale Connection" (ErrStaleConn "Stale Connection"))
    , pure (errFrame "Maximum Connections Exceeded" (ErrMaxConnsEx "Maximum Connections Exceeded"))
    , pure (errFrame "Slow Consumer" (ErrSlowConsumer "Slow Consumer"))
    , pure (errFrame "Maximum Payload Violation" (ErrMaxPayload "Maximum Payload Violation"))
    , pure (errFrame "Invalid Subject" (ErrInvalidSubject "Invalid Subject"))
    , genPermViolationFrame
    ]
  where
    errFrame reason err =
      ValidFrame ("-ERR '" <> reason <> "'\r\n") (ParsedErr err)

genPermViolationFrame :: Gen ValidFrame
genPermViolationFrame = do
  direction <- elements ["Subscription", "Publish"]
  subjectName <- genSubject
  let reason = "Permissions Violation For " <> direction <> " To " <> subjectName
  pure (ValidFrame ("-ERR '" <> reason <> "'\r\n") (ParsedErr (ErrPermViolation reason)))

genInfoFrame :: Gen ValidFrame
genInfoFrame = do
  infoValue <- genInfo
  pure (ValidFrame (renderInfoFrame infoValue) (ParsedInfo infoValue))

genMsgFrame :: Gen ValidFrame
genMsgFrame = do
  subjectName <- genSubject
  sidValue <- genAlphaNumBytes 1 12
  replySubject <- frequency [(2, pure Nothing), (3, Just <$> genSubject)]
  useHeaders <- arbitrary
  if useHeaders
    then do
      rawHeaders <- genHeaders
      payloadValue <- genPayload 1 128
      let renderedMsg = Msg subjectName sidValue replySubject (Just payloadValue) (Just rawHeaders)
          parsedMsg =
            Msg
              subjectName
              sidValue
              replySubject
              (Just payloadValue)
              (Just (normalizeHeaders rawHeaders))
      pure (ValidFrame (renderHMsgFrame renderedMsg) (ParsedMsg parsedMsg))
    else do
      maybePayload <- frequency [(1, pure Nothing), (4, Just <$> genPayload 1 128)]
      let parsedMsg = Msg subjectName sidValue replySubject maybePayload Nothing
      pure (ValidFrame (renderMsgFrame parsedMsg) (ParsedMsg parsedMsg))

genInfo :: Gen Info
genInfo =
  Info
    <$> genSafeText 1 16
    <*> genSafeText 1 12
    <*> genSafeText 1 8
    <*> genSafeText 1 16
    <*> chooseInt (1, 65535)
    <*> chooseInt (0, 1048576)
    <*> elements [0 .. 5]
    <*> frequency [(2, pure Nothing), (3, Just <$> chooseInt (1, 100000))]
    <*> frequency [(2, pure Nothing), (3, Just <$> genSafeText 1 16)]
    <*> genOptional arbitrary
    <*> genOptional arbitrary
    <*> genOptional (listOf1 (genSafeText 1 24))
    <*> genOptional arbitrary
    <*> genOptional arbitrary

genOptional :: Gen a -> Gen (Maybe a)
genOptional generator =
  frequency [(2, pure Nothing), (3, Just <$> generator)]

genHeaders :: Gen [(BS.ByteString, BS.ByteString)]
genHeaders =
  sized $ \size ->
    do
      pairCount <- chooseInt (1, max 1 (min 4 (size + 1)))
      vectorOf pairCount ((,) <$> genHeaderToken <*> genHeaderValue)

genHeaderToken :: Gen BS.ByteString
genHeaderToken =
  genBytesFromAlphabet 1 12 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-"

genHeaderValue :: Gen BS.ByteString
genHeaderValue =
  genBytesFromAlphabet 1 32 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_/.: "

genSubject :: Gen BS.ByteString
genSubject =
  frequency
    [ (1, pure ">")
    , (1, pure "*")
    , (6, genDottedSubject)
    ]

genDottedSubject :: Gen BS.ByteString
genDottedSubject = do
  segmentCount <- chooseInt (1, 4)
  segments <- vectorOf segmentCount genSubjectSegment
  useTailWildcard <- arbitrary
  let baseSubject = B8.intercalate "." segments
  pure
    ( if useTailWildcard
        then baseSubject <> ".>"
        else baseSubject
    )

genSubjectSegment :: Gen BS.ByteString
genSubjectSegment =
  frequency
    [ (1, pure "*")
    , (5, genBytesFromAlphabet 1 12 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_$/" )
    ]

genSafeText :: Int -> Int -> Gen BS.ByteString
genSafeText minLength maxLength =
  genBytesFromAlphabet minLength maxLength "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.:/"

genAlphaNumBytes :: Int -> Int -> Gen BS.ByteString
genAlphaNumBytes minLength maxLength =
  genBytesFromAlphabet minLength maxLength "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

genPayload :: Int -> Int -> Gen BS.ByteString
genPayload minLength maxLength =
  genBytesFromAlphabet minLength maxLength payloadAlphabet
  where
    payloadAlphabet =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 !?._-/*+\r\n"

genBytesFromAlphabet :: Int -> Int -> BS.ByteString -> Gen BS.ByteString
genBytesFromAlphabet minLength maxLength alphabet = do
  targetLength <- chooseInt (minLength, maxLength)
  bytes <- vectorOf targetLength (elements (BS.unpack alphabet))
  pure (BS.pack bytes)

renderMsgFrame :: Msg -> BS.ByteString
renderMsgFrame msgValue =
  BS.concat
    [ "MSG "
    , Msg.subject msgValue
    , " "
    , Msg.sid msgValue
    , " "
    , maybe "" (<> " ") (Msg.replyTo msgValue)
    , decimalBytes payloadLength
    , "\r\n"
    , payloadBytes
    , "\r\n"
    ]
  where
    payloadBytes = fromMaybe "" (Msg.payload msgValue)
    payloadLength = BS.length payloadBytes

renderHMsgFrame :: Msg -> BS.ByteString
renderHMsgFrame msgValue =
  BS.concat
    [ "HMSG "
    , Msg.subject msgValue
    , " "
    , Msg.sid msgValue
    , " "
    , maybe "" (<> " ") (Msg.replyTo msgValue)
    , decimalBytes (BS.length headerBytes)
    , " "
    , decimalBytes (BS.length headerBytes + BS.length payloadBytes)
    , "\r\n"
    , headerBytes
    , payloadBytes
    , "\r\n"
    ]
  where
    headerBytes = renderHeaderBlock (fromMaybe [] (Msg.headers msgValue))
    payloadBytes = fromMaybe "" (Msg.payload msgValue)

renderHeaderBlock :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
renderHeaderBlock headerPairs =
  BS.concat ("NATS/1.0\r\n" : foldr appendHeader ["\r\n"] headerPairs)
  where
    appendHeader (headerKey, headerValue) acc =
      [headerKey, ":", headerValue, "\r\n"] ++ acc

normalizeHeaders :: [(BS.ByteString, BS.ByteString)] -> [(BS.ByteString, BS.ByteString)]
normalizeHeaders =
  map (bimap trimHorizontalSpace trimHorizontalSpace)

trimHorizontalSpace :: BS.ByteString -> BS.ByteString
trimHorizontalSpace =
  BS.dropWhile isHorizontalSpaceByte . dropWhileEnd isHorizontalSpaceByte

dropWhileEnd :: (Word8 -> Bool) -> BS.ByteString -> BS.ByteString
dropWhileEnd predicate =
  BS.reverse . BS.dropWhile predicate . BS.reverse

isHorizontalSpaceByte :: Word8 -> Bool
isHorizontalSpaceByte byte =
  byte == 32 || byte == 9

renderInfoFrame :: Info -> BS.ByteString
renderInfoFrame infoValue =
  BS.concat
    [ "INFO {"
    , field "server_id" (quote (server_id infoValue))
    , ","
    , field "version" (quote (version infoValue))
    , ","
    , field "go" (quote (go infoValue))
    , ","
    , field "host" (quote (host infoValue))
    , ","
    , field "port" (decimalBytes (port infoValue))
    , ","
    , field "max_payload" (decimalBytes (max_payload infoValue))
    , ","
    , field "proto" (decimalBytes (proto infoValue))
    , maybeField "client_id" (decimalBytes <$> client_id infoValue)
    , maybeField "nonce" (quote <$> nonce infoValue)
    , maybeField "auth_required" (jsonBool <$> auth_required infoValue)
    , maybeField "tls_required" (jsonBool <$> tls_required infoValue)
    , maybeField "connect_urls" (jsonArray <$> connect_urls infoValue)
    , maybeField "ldm" (jsonBool <$> ldm infoValue)
    , maybeField "headers" (jsonBool <$> headers infoValue)
    , "}\r\n"
    ]

field :: BS.ByteString -> BS.ByteString -> BS.ByteString
field key value = quote key <> ":" <> value

maybeField :: BS.ByteString -> Maybe BS.ByteString -> BS.ByteString
maybeField _ Nothing        = ""
maybeField key (Just value) = "," <> field key value

quote :: BS.ByteString -> BS.ByteString
quote value = "\"" <> value <> "\""

jsonBool :: Bool -> BS.ByteString
jsonBool True  = "true"
jsonBool False = "false"

jsonArray :: [BS.ByteString] -> BS.ByteString
jsonArray values = "[" <> B8.intercalate "," (map quote values) <> "]"

decimalBytes :: Int -> BS.ByteString
decimalBytes = B8.pack . show
