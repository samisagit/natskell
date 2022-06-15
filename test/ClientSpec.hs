module ClientSpec (spec) where

import           Client
import           Control.Exception
import qualified Docker.Client     as DC
import           Integration
import           Test.Hspec

spec :: Spec
spec = do
  integration

withNATSConnection :: (DC.ContainerID -> IO ()) -> IO ()
withNATSConnection = bracket startNATS stopNATS

integration = do
  around withNATSConnection $ do
    describe "Client" $ do
      it "connects successfully" $ \_ -> do
        connect $ Api "0.0.0.0" "4222"
