module Main where

import qualified AuthSpec
import qualified ClientSpec
import           Test.Hspec

main :: IO ()
main = hspec $ do
  ClientSpec.spec
  AuthSpec.spec
