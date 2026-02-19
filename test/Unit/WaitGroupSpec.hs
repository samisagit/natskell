module WaitGroupSpec (spec) where

import           Control.Exception (throwIO)
import           System.Timeout    (timeout)
import           Test.Hspec
import           WaitGroup

spec :: Spec
spec = describe "WaitGroup" $ do
  it "unblocks wait when a forked action throws" $ do
    wg <- newWaitGroup 1
    _ <- forkWaitGroup wg (throwIO (userError "boom"))
    result <- timeout 1000000 (wait wg)
    result `shouldBe` Just ()
