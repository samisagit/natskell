module ClientSpec (spec) where

import           Client
import           Control.Exception
import qualified Docker.Client     as DC
import           NatsWrappers
import           Test.Hspec

spec :: Spec
spec = do
  sys

withNATSConnection :: (DC.ContainerID -> IO ()) -> IO ()
withNATSConnection = bracket startNATS stopNATS

sys = do
  around withNATSConnection $ do
    describe "Client" $ do
      describe "systest" $ do
        it "connects successfully" $ \_ -> do
          connect $ Api "0.0.0.0" "4222"
