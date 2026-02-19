{-# LANGUAGE OverloadedStrings #-}

module SidSpec (spec) where

import           Sid
import           Test.Hspec

spec :: Spec
spec = describe "SID generation" $ do
  it "increments sequentially from the initial counter" $ do
    let (sid1, counter1) = nextSID initialSIDCounter
        (sid2, counter2) = nextSID counter1
    sid1 `shouldBe` "1"
    sid2 `shouldBe` "2"
    counter2 `shouldBe` 2
