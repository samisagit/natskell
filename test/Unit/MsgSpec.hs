{-# LANGUAGE OverloadedStrings #-}

module MsgSpec (spec) where

import           Control.Monad
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Fixtures
import           Lib.Parser
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Msg


spec :: Spec
spec = do
  explicit
  applicative

cases :: [(BS.ByteString, Msg)]
cases = [
  ("MSG FOO 13 BAR 17\r\nsome payload bits\r\n", Msg "FOO" "13" (Just "BAR") 17 (Just "some payload bits")),
  ("MSG FOO 13 BAR 0\r\n", Msg "FOO" "13" (Just "BAR") 0 Nothing),
  ("MSG FOO 13 17\r\nsome payload bits\r\n", Msg "FOO" "13" Nothing 17 (Just"some payload bits")),
  ("MSG FOO 13 0\r\n", Msg "FOO" "13" Nothing 0 Nothing),
  ("MSG FOO 13 20\r\nmulti\r\nline\r\npayload\r\n", Msg "FOO" "13" Nothing 20 (Just "multi\r\nline\r\npayload")),
  ("MSG FOO.BAR.BAZ 13 IN.*.BOX.> 0\r\n", Msg "FOO.BAR.BAZ" "13" (Just "IN.*.BOX.>") 0 Nothing)
  ]

explicit = parallel $ do
  forM_ cases $ \(input, expected) -> do
    describe "specific parser" $ do
      it (printf "correctly parses %s" (show input)) $ do
        let output = runParser msgParser input
        let result = fmap fst output
        let rest = fmap snd output
        case result of
          Just (ParsedMsg a) -> a `shouldBe` expected
          Nothing            -> error "parser did not return MSG type"
        case rest of
          Just "" -> return ()
          _       -> error "parser did not consume all tokens"
    describe "generic parser" $ do
      it (printf "correctly parses %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Just (ParsedMsg expected)

applicative = parallel $ do
  describe "generated" $ do
    describe "MSG parser" $ do
      let msgs = map uncurry (Msg
            <$> subjectCases
            <*> sidCases
            <*> maybeify subjectCases)
            <*> payloadCases -- because byteCount and payload are in valid pairs we needed to uncurry this
      let proto = map buildProtoInput msgs
      let pairs = zip proto msgs

      forM_ pairs $ \(input, want) -> do
        it (printf "correctly parses %s" (show input)) $ do
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

