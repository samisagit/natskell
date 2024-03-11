{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           API
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

      (lock, assertion) <- matcherMsg "Hello World"
      let subj = "SOME.EVENT"
      sid <- sub
        c
        [ subWithSubject subj,
          subWithCallback assertion
        ]

      mockPub harness $ foldl BS.append "MSG " [ subj,  " ", sid, " ", "11\r\nHello World\r\n+OK\r\n" ]
      join . atomically $ takeTMVar lock
      return ()

    it "subscribes to its reply before publishing" $ do
      (c, harness) <- newNatsHarness
      pub c [
        pubWithSubject "SOME.ENDPOINT",
        pubWithPayload "Hello World",
        pubWithReplyCallback (\_ -> return ())
        ]

      (_:sub:pub:_) <- sentMessages harness
      subjectFromSub sub `shouldBe` replyToFromPub pub

subjectFromSub :: BS.ByteString -> BS.ByteString
subjectFromSub s = head . tail $ BS.split _space s

replyToFromPub :: BS.ByteString -> BS.ByteString
replyToFromPub s = head . tail . tail $ BS.split _space s

