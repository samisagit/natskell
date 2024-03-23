module CallbackAssertions where

import           API
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString        as BS
import           Test.Hspec

payloadAssertion :: Maybe BS.ByteString -> (Msg -> Expectation)
payloadAssertion matcher (Msg _ _ _ msg _) = msg `shouldBe` matcher

headerAssertion :: Maybe [(BS.ByteString, BS.ByteString)] -> (Msg -> Expectation)
headerAssertion matcher (Msg _ _ _ _ headers) = headers `shouldBe` matcher

subjectAssertion :: BS.ByteString -> (Msg -> Expectation)
subjectAssertion matcher (Msg sub _ _ _ _) = sub `shouldBe` matcher

asyncAssert :: [Msg -> Expectation] -> IO (TMVar Expectation, Msg -> IO ())
asyncAssert e = do
  lock <- newEmptyTMVarIO
  let callback msg = atomically $ putTMVar lock (forM_ e $ \assert -> assert msg)
  return (lock, callback)
