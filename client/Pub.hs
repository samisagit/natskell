{-# LANGUAGE OverloadedStrings #-}

module Pub where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString    as BS
import           Data.UUID
import           Data.UUID.V4
import           Nats.Nats
import           Sub
import           Types
import           Types.Msg
import           Types.Pub
import           Unsub

type PubOptions = (Subject, BS.ByteString, Maybe (Msg -> IO ()))

applyPubOptions :: PubOptions -> [PubOptions -> PubOptions] -> PubOptions
applyPubOptions = foldl (flip ($))

defaultPubOptions :: PubOptions
defaultPubOptions = ("", "", Nothing)

pub :: NatsConn a => NatsAPI a -> [PubOptions -> PubOptions] -> IO ()
pub nats options = do
  let (subject, payload, callback) = applyPubOptions defaultPubOptions options
  sid <- toASCIIBytes <$> nextRandom
  case callback of
    Nothing -> sendBytes nats $ Pub subject Nothing Nothing (Just payload)
    Just cb -> do
      let replyTo = foldr BS.append "" [subject, ".REPLY.", sid]
      sub nats [
        subWithSID sid,
        subWithSubject replyTo,
        subWithCallback (replyCallback nats cb sid subject)
        ]
      sendBytes nats $ Pub subject (Just replyTo) Nothing (Just payload)
      void . forkIO $ do
        _ <- threadDelay 10000000
        unsub nats sid replyTo

pubWithSubject :: Subject -> PubOptions -> PubOptions
pubWithSubject subject (_, payload, callback) = (subject, payload, callback)

pubWithPayload :: BS.ByteString -> PubOptions -> PubOptions
pubWithPayload payload (subject, _, callback) = (subject, payload, callback)

pubWithReplyCallback :: (Msg -> IO ()) -> PubOptions -> PubOptions
pubWithReplyCallback callback (subject, payload, _) = (subject, payload, Just callback)

replyCallback :: NatsConn a => NatsAPI a -> (Msg -> IO ()) -> BS.ByteString -> BS.ByteString -> (Msg -> IO ())
replyCallback nats callback sid subject msg = do
  unsub nats sid subject
  callback msg
