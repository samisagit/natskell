module ClientSpec (spec) where

import           Client
import           Control.Exception
import           Control.Monad
import qualified Docker.Client     as DC
import           NatsWrappers
import           Test.Hspec

spec :: Spec
spec = do
  sys

withNATSConnection :: String -> ((DC.ContainerID, String, Int) -> IO ()) -> IO ()
withNATSConnection tag = bracket (startNATS tag) stopNATS

versions :: [String]
versions = ["latest", "2.9.8", "2.9.6"]

sys = parallel $ do
  describe "Client" $ do
    describe "systest" $ do
      forM_ versions $ \version ->
        around (withNATSConnection version) $ do
          it "connects successfully" $ \(_, host, port) -> do
            connect host port 10
