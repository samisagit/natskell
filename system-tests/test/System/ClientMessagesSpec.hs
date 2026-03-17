{-# LANGUAGE OverloadedStrings #-}

module ClientMessagesSpec (spec) where

import           API                (Client (..), MsgView (..), withPayload)
import           Client
import           Control.Concurrent
import qualified Data.ByteString    as BS
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec = do
  clientSystemTest "2aa35ca5-5e75-4a47-b3af-97dbe2881151" "messages are sent and received" $
    messageTest (Just "HELLO")
  clientSystemTest "9f24cb8e-c4a1-4fe5-9e87-dbb0bc3f79a2" "messages without payload are sent and received" $
    messageTest Nothing

messageTest :: Maybe BS.ByteString -> [ConfigOption] -> Endpoints -> IO ()
messageTest payload loggerOptions (Endpoints natsHost natsPort) = do
  let topic = "SOME.TOPIC"
  lock <- newEmptyMVar
  sidBox <- newEmptyMVar
  wg <- newWaitGroup 1
  assertClient <- newClient [(natsHost, natsPort)] $
    withConnectName "0dfe787e-383b-4cb8-a73f-8474f4cc0497"
      : loggerOptions
  promptClient <- newClient [(natsHost, natsPort)] $
    withConnectName "0e81e61a-932f-4036-9cdd-9a65fb4ed829"
      : loggerOptions
  subscribe assertClient topic [] $ \msg -> do
    case msg of
      Nothing -> error "Received empty message"
      Just msg' -> do
        unsubscribe assertClient (sid msg')
        putMVar lock msg'
        putMVar sidBox (sid msg')
        done wg
  flush assertClient
  let publishOptions = maybe [] (\payloadValue -> [withPayload payloadValue]) payload
  publish promptClient topic publishOptions
  wait wg
  msg <- takeMVar lock
  sid' <- takeMVar sidBox
  msg `shouldBe` MsgView topic sid' Nothing payload Nothing
  close assertClient
  close promptClient
