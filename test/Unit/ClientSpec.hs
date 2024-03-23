{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           API
import           CallbackAssertions
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString        as BS
import           Data.Word8
import           Harness
import           Test.Hspec

spec :: Spec
spec = do
  cases

cases :: Spec
cases =  describe "Client" $ do
    it "processes a single message" $ do
      (c, harness) <- newNatsHarness
      (lock, assertion) <- asyncAssert [
        payloadAssertion $ Just "Hello World",
        headerAssertion Nothing
        ]
      let subj = "SOME.EVENT"
      sid <- sub
        c
        [ subWithSubject subj,
          subWithCallback assertion
        ]
      mockPub harness $ foldl BS.append "MSG " [ subj,  " ", sid, " ", "11\r\nHello World\r\n+OK\r\n" ]
      join . atomically $ takeTMVar lock

    it "publishes to its connection" $ do
      (c, harness) <- newNatsHarness
      pub c [
        pubWithSubject "SOME.SUBJECT",
        pubWithPayload "Hello World"
        ]
      (_:pub:_) <- sentMessages harness
      pub `shouldBe` "PUB SOME.SUBJECT 11\r\nHello World\r\n"

    it "subscribes to its reply before publishing" $ do
      (c, harness) <- newNatsHarness
      pub c [
        pubWithSubject "SOME.ENDPOINT",
        pubWithPayload "Hello World",
        pubWithReplyCallback (\_ -> return ())
        ]
      (_:sub:pub:_) <- sentMessages harness
      subjectFromSub sub `shouldBe` replyToFromPub pub

    it "publishes with headers" $ do
      (c, harness) <- newNatsHarness
      pub c [
        pubWithSubject "SOME.SUBJECT",
        pubWithPayload "Hello World",
        pubWithHeaders [("key", "val")]
        ]
      (_:pub:_) <- sentMessages harness
      pub `shouldBe` "HPUB SOME.SUBJECT 21 32\r\nNATS/1.0\r\nkey:val\r\n\r\nHello World\r\n"

    it "publishes with multiple headers" $ do
      (c, harness) <- newNatsHarness
      pub c [
        pubWithSubject "SOME.SUBJECT",
        pubWithPayload "Hello World",
        pubWithHeaders [("key", "val"), ("key2", "val2")]
        ]
      (_:pub:_) <- sentMessages harness
      pub `shouldBe` "HPUB SOME.SUBJECT 32 43\r\nNATS/1.0\r\nkey:val\r\nkey2:val2\r\n\r\nHello World\r\n"

    it "processes a single message with headers" $ do
      (c, harness) <- newNatsHarness
      (lock, assertion) <- asyncAssert [
        payloadAssertion $ Just "Hello World",
        headerAssertion $ Just [("key", "val")]
        ]
      let subj = "SOME.EVENT"
      sid <- sub
        c
        [ subWithSubject subj,
          subWithCallback assertion
        ]
      mockPub harness $ foldl BS.append "HMSG " [ subj,  " ", sid, " ", "21 32\r\nNATS/1.0\r\nkey:val\r\n\r\nHello World\r\n+OK\r\n" ]
      join . atomically $ takeTMVar lock

    it "processes a single message with multiple headers" $ do
      (c, harness) <- newNatsHarness
      (lock, assertion) <- asyncAssert [
        payloadAssertion $ Just "Hello World",
        headerAssertion $ Just [("key", "val"),("key2","val2")]
        ]
      let subj = "SOME.EVENT"
      sid <- sub
        c
        [ subWithSubject subj,
          subWithCallback assertion
        ]
      mockPub harness $ foldl BS.append "HMSG " [ subj,  " ", sid, " ", "32 43\r\nNATS/1.0\r\nkey:val\r\nkey2:val2\r\n\r\nHello World\r\n+OK\r\n" ]
      join . atomically $ takeTMVar lock

subjectFromSub :: BS.ByteString -> BS.ByteString
subjectFromSub s = head . tail $ BS.split _space s

replyToFromPub :: BS.ByteString -> BS.ByteString
replyToFromPub s = head . tail . tail $ BS.split _space s

