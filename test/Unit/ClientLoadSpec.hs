{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientLoadSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import           Data.Text.Encoding     (encodeUtf8)
import qualified Nats.Nats              as N
import           Test.Hspec

instance N.NatsConn (TMVar BS.ByteString) where
  recv sock x = do
    bs <- atomically $ takeTMVar sock
    let (a, b) = BS.splitAt x bs
    atomically $ putTMVar sock b
    if BS.length a == 0
      then return Nothing
      else return (Just a)
  send _ _ = do
    return () -- we seed the data to the 'socket' in the test

spec :: Spec
spec = do
  cases

cases = parallel $ do
  describe "Client" $ do
    it "processes a single message" $ do
      let matchers        = map (packStr . show) [1]
      let msgs            = foldr (BS.append . matcherMsg) "" matchers
      let assertions      = map matcherAssertion matchers
      asyncAssertions     <- mapM asyncAssert assertions
      let keyedAssertions = zip matchers asyncAssertions
      BS.length msgs < N.socketReadLength `shouldBe` True
      performAssertions msgs keyedAssertions
    it "processes messages from multiple socket reads" $ do
      let matchers        = map (packStr . show) [1..62]
      let msgs            = foldr (BS.append . matcherMsg) "" matchers
      let assertions      = map matcherAssertion matchers
      asyncAssertions     <- mapM asyncAssert assertions
      let keyedAssertions = zip matchers asyncAssertions
      BS.length msgs > N.socketReadLength `shouldBe` True
      performAssertions msgs keyedAssertions

performAssertions :: BS.ByteString -> [(BS.ByteString, (TMVar Expectation, Msg -> IO ()))] -> IO ()
performAssertions msgs keyedAssertions = do
  -- dump all the msgs onto the 'socket'
  socket <- newTMVarIO msgs
  nats <- N.nats socket
  -- subscribe to all matchers
  forM_ keyedAssertions $ \(matcher, (_, callback)) -> sub nats [
    subWithSubject matcher,
    subWithSID matcher,
    subWithCallback callback
    ]
  -- process the msgs
  handShake nats
  -- wait for the locks to release
  forM_ keyedAssertions $ \(_, (lock, _)) -> join . atomically $ takeTMVar lock


matcherMsg :: BS.ByteString -> BS.ByteString
matcherMsg matcher = BS.concat ["MSG ", matcher, " ", matcher, " ", (packStr . show) byteLength, "\r\n", matcher, "\r\n"]
  where byteLength = BS.length matcher

matcherAssertion :: BS.ByteString -> (Msg -> Expectation)
matcherAssertion matcher msg = msg `shouldBe` Msg matcher matcher Nothing (Just matcher) Nothing

asyncAssert :: (Msg -> Expectation) -> IO (TMVar Expectation, Msg -> IO ())
asyncAssert e = do
  lock <- newEmptyTMVarIO
  let callback msg = atomically $ putTMVar lock (e msg)
  return (lock, callback)

packStr :: String -> BS.ByteString
packStr = encodeUtf8 . Text.pack

