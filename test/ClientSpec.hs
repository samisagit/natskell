module ClientSpec (spec) where

import           Client
import           Control.Exception
import qualified Docker.Client      as DC
import           NatsWrappers
import qualified Network.Simple.TCP as TCP
import           Test.Hspec

spec :: Spec
spec = do
  sys

withNATSConnection :: ((DC.ContainerID, String, Int) -> IO ()) -> IO ()
withNATSConnection = bracket startNATS stopNATS

sys = do
  around withNATSConnection $ do
    describe "Client" $ do
      describe "systest" $ do
        it "connects successfully" $ \(_, host, port) -> do
          connect host port 10
