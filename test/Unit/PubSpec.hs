{-# LANGUAGE OverloadedStrings #-}

module PubSpec (spec) where

import           ASCII                     as A
import           ASCII.Char                as AC
import           Control.Monad
import qualified Data.ByteString           as BS
import           Data.Maybe
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import           Fixtures
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Pub

spec :: Spec
spec = do
  explicit
  generated

cases :: [(Pub, BS.ByteString, String)]
cases = [
  (Pub "FOO.BAR" Nothing Nothing, "PUB FOO.BAR 0\r\n", "without max messages"),
  (Pub "FOO.BAR" Nothing (Just "Some payload bits"), "PUB FOO.BAR 17\r\nSome payload bits\r\n", "with max messages"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") Nothing, "PUB FOO.BAR FOO.BAR.REPLY 0\r\n", "with max messages"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") (Just "Some payload bits"), "PUB FOO.BAR FOO.BAR.REPLY 17\r\nSome payload bits\r\n", "with max messages")
  ]

explicit = parallel $ do
  forM_ cases $ \(input, expected, caseName) ->
    it (printf "correctly transforms %s" caseName) $ do
      transform input `shouldBe` expected

generated = parallel $ do
  describe "generated" $ do
    forM_ subjectCases $ \subject -> do
      forM_ (maybeify subjectCases) $ \replyTo -> do
        forM_ maxPayloadCases $ \byteCount -> do
          let payload = payloadGen byteCount
          let output = foldr BS.append "" [
                "PUB ",
                subject,
                " ",
                collapseMaybeBS replyTo " ",
                intToByteString byteCount,
                "\r\n",
                collapseMaybeBS payload "\r\n"
                ]
          it (printf "correctly transforms %s" (show output)) $ do
            transform (Pub subject replyTo payload) `shouldBe` output

infASCII = cycle AC.allCharacters

payloadGen n
  | n == 0 = Nothing
  | n > 0 = Just . encodeUtf8 . T.pack . map A.charToUnicode $ take n infASCII

collapseMaybeBS :: Maybe BS.ByteString -> BS.ByteString -> BS.ByteString
collapseMaybeBS (Just a) sep = BS.append a sep
collapseMaybeBS Nothing _    = ""

intToByteString = encodeUtf8 . T.pack . show
