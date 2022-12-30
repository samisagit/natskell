{-# LANGUAGE OverloadedStrings #-}

module MsgSpec (spec) where

import           Control.Monad
import qualified Data.ByteString    as BS
import           Data.Maybe
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Fixtures
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Msg

spec :: Spec
spec = do
  cases

explicitCases :: [(BS.ByteString, Msg)]
explicitCases = [
  ("MSG FOO 13 BAR 17\r\nsome payload bits\r\n", Msg "FOO" "13" (Just "BAR") (Just "some payload bits") Nothing),
  ("MSG FOO 13 BAR 0\r\n", Msg "FOO" "13" (Just "BAR") Nothing Nothing),
  ("MSG FOO 13 17\r\nsome payload bits\r\n", Msg "FOO" "13" Nothing (Just"some payload bits") Nothing),
  ("MSG FOO 13 0\r\n", Msg "FOO" "13" Nothing Nothing Nothing),
  ("MSG FOO 13 20\r\nmulti\r\nline\r\npayload\r\n", Msg "FOO" "13" Nothing (Just "multi\r\nline\r\npayload") Nothing),
  ("MSG FOO.BAR.BAZ 13 IN.*.BOX.> 0\r\n", Msg "FOO.BAR.BAZ" "13" (Just "IN.*.BOX.>") Nothing Nothing),
  ("HMSG FOO 13 BAR 24 41\r\nNATS/1.0\r\nKEY: VALUE\r\n\r\nsome payload bits\r\n", Msg "FOO" "13" (Just "BAR") (Just "some payload bits") (Just [("KEY", "VALUE")])),
  ("HMSG FOO 13 24 44\r\nNATS/1.0\r\nKEY: VALUE\r\n\r\nmulti\r\nline\r\npayload\r\n", Msg "FOO" "13" Nothing (Just "multi\r\nline\r\npayload") (Just [("KEY", "VALUE")]))
  ]

generatedCases :: [(BS.ByteString, Msg)]
generatedCases = zip (map buildProtoInput msgs) msgs
  where
    msgs = (Msg
            <$> subjectCases
            <*> sidCases
            <*> maybeify subjectCases)
            <*> payloadCases
            <*> maybeify headerCases

cases = parallel $ do
  describe "generic parser" $ do
    forM_ explicitCases $ \(input, want) -> do
      it (printf "correctly parses explicit case %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Just (ParsedMsg want)
    forM_ generatedCases $ \(input, want) -> do
      it (printf "correctly parses generated case %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Just (ParsedMsg want)

buildProtoInput :: Msg -> BS.ByteString
buildProtoInput m = do
  let headerProto = buildHeaderInput . headers $ m
  foldr BS.append "" [
    if isNothing . headers $ m then "" else "H",
    "MSG",
    " ",
    subject m,
    " ",
    sid m,
    " ",
    collapseNothing (replyTo m) " ",
    if isNothing . headers $ m then "" else packStr' (printf "%v" (BS.length headerProto)),
    if isNothing . headers $ m then "" else " ",
    packStr' (printf "%v" (BS.length headerProto + (payloadLength . payload $ m ))),
    if isNothing . headers $ m then "" else "\r\n",
    headerProto,
    if isJust . headers $ m then "" else "\r\n",
    collapseNothing (payload m) "\r\n"
    ]

buildHeaderInput :: Maybe [(BS.ByteString, BS.ByteString)] -> BS.ByteString
buildHeaderInput Nothing   = ""
buildHeaderInput (Just xs) = foldr BS.append "" ["NATS/1.0\r\n", headerPairProto xs, "\r\n"]

headerPairProto :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
headerPairProto []          = ""
headerPairProto ((k, v):xs) = BS.append (foldr BS.append "" [k, ": ", v, "\r\n"]) (headerPairProto xs)

collapseNothing :: Maybe BS.ByteString -> BS.ByteString -> BS.ByteString
collapseNothing mbs suffix = case mbs of
  Just a  -> BS.append a suffix
  Nothing -> ""

payloadLength :: Maybe BS.ByteString -> Int
payloadLength Nothing   = 0
payloadLength (Just xs) = BS.length xs

packStr' :: String -> BS.ByteString
packStr' = encodeUtf8 . T.pack

