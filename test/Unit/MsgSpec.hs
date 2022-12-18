{-# LANGUAGE OverloadedStrings #-}

module MsgSpec (spec) where

import           Control.Monad
import qualified Data.ByteString    as BS
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
  ("MSG FOO 13 BAR 17\r\nsome payload bits\r\n", Msg "FOO" "13" (Just "BAR") 17 (Just "some payload bits")),
  ("MSG FOO 13 BAR 0\r\n", Msg "FOO" "13" (Just "BAR") 0 Nothing),
  ("MSG FOO 13 17\r\nsome payload bits\r\n", Msg "FOO" "13" Nothing 17 (Just"some payload bits")),
  ("MSG FOO 13 0\r\n", Msg "FOO" "13" Nothing 0 Nothing),
  ("MSG FOO 13 20\r\nmulti\r\nline\r\npayload\r\n", Msg "FOO" "13" Nothing 20 (Just "multi\r\nline\r\npayload")),
  ("MSG FOO.BAR.BAZ 13 IN.*.BOX.> 0\r\n", Msg "FOO.BAR.BAZ" "13" (Just "IN.*.BOX.>") 0 Nothing)
  ]

generatedCases :: [(BS.ByteString, Msg)]
generatedCases = zip (map buildProtoInput msgs) msgs
  where
    msgs = map uncurry (Msg
            <$> subjectCases
            <*> sidCases
            <*> maybeify subjectCases)
            <*> payloadCases -- because byteCount and payload are in valid pairs we needed to uncurry this

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
buildProtoInput m = foldr BS.append "" [
  "MSG",
  " ",
  subject m,
  " ",
  sid m,
  " ",
  collapseNothing (replyTo m) " ",
  packStr' (printf "%v" (byteCount m)),
  "\r\n",
  collapseNothing (payload m) "\r\n"
  ]

collapseNothing :: Maybe BS.ByteString -> BS.ByteString -> BS.ByteString
collapseNothing mbs suffix = case mbs of
  Just a  -> BS.append a suffix
  Nothing -> ""

packStr' :: String -> BS.ByteString
packStr' = encodeUtf8 . T.pack

