{-# LANGUAGE OverloadedStrings #-}

module Client2Spec (spec) where
import           Client2
import           Control.Concurrent (forkIO)
import           Control.Exception
import           Control.Monad      (forM_)
import qualified Data.Text          as Text
import qualified Docker.Client      as DC
import           GHC.Conc           (threadDelay)
import           GHC.MVar
import           NatsWrappers
import           Test.Hspec
import           Text.Printf        (printf)
import WaitGroup

spec :: Spec
spec = do
  sys

versions = ["latest", "2.9.8", "2.9.6"]

sys = parallel $ do
  forM_ versions $ \version ->
    describe (printf "client 2 (nats:%s)" version) $ do
      around (withNATSConnection version) $ do
        it "receives others messages" $ \(_, host, port) -> do
          socket <- defaultConn host port
          lock <- newEmptyMVar
          sidBox <- newEmptyMVar
          forkIO . withNats [] socket $ \client -> do
            sub client "SOME.TOPIC" $ \x -> do
              unsub client (sid x)
              stop client
              putMVar lock x
              putMVar sidBox (sid x)

          sleep 1
          socket' <- defaultConn host port
          forkIO . withNats [] socket' $ \client -> do
            pub client [pubWithSubject "SOME.TOPIC", pubWithPayload "HELLO"]
            stop client

          msg <- takeMVar lock
          sid' <- takeMVar sidBox
          msg `shouldBe` Msg "SOME.TOPIC" sid' Nothing (Just "HELLO") Nothing
        it "can have multiple subscriptions" $ \(_, host, port) -> do
          socket <- defaultConn host port
          wg <- newWaitGroup 2
          clientBox <- newEmptyMVar
          forkIO . withNats [] socket $ \client -> do
            sub client "SOME.TOPIC" $ \x -> do
              unsub client (sid x)
              done wg
            sub client "SOME.OTHER.TOPIC" $ \x -> do
              unsub client (sid x)
              putMVar clientBox client
              done wg

          sleep 1
          socket' <- defaultConn host port
          forkIO . withNats [] socket' $ \client -> do
            pub client [pubWithSubject "SOME.TOPIC", pubWithPayload "HELLO"]
            pub client [pubWithSubject "SOME.OTHER.TOPIC", pubWithPayload "HELLO"]
            stop client

          wait wg
          client <- takeMVar clientBox
          stop client

withNATSConnection :: Text.Text -> ((DC.ContainerID, String, Int) -> IO ()) -> IO ()
withNATSConnection tag = bracket (startNATS tag) stopNATS

sleep :: Int -> IO ()
sleep = threadDelay . (*100000)
