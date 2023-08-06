{-# LANGUAGE OverloadedStrings #-}

module Pub where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString    as BS
import           Nats.Nats
import           Sub
import           Types
import           Types.Msg
import           Types.Pub
import           Unsub

type PubOptions = (Subject, BS.ByteString, Maybe (Msg -> IO ()), SID)

applyPubOptions :: PubOptions -> [PubOptions -> PubOptions] -> PubOptions
applyPubOptions = foldl (flip ($))

defaultPubOptions :: PubOptions
defaultPubOptions = ("", "", Nothing, "")

pub :: NatsConn a => NatsAPI a -> [PubOptions -> PubOptions] -> IO ()
pub nats options = do
  let (subject, payload, callback, sid) = applyPubOptions defaultPubOptions options
  case callback of
    Nothing -> sendBytes nats $ Pub subject Nothing Nothing (Just payload)
    Just cb -> do
      let replyTo = BS.append subject ".REPLY" -- TODO: this subject needs to be unique, so user created subs don't get removed on reciept, or perhaps just the SID need to be unique, need to check the use of that
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
pubWithSubject subject (_, payload, callback, sid) = (subject, payload, callback, sid)

pubWithPayload :: BS.ByteString -> PubOptions -> PubOptions
pubWithPayload payload (subject, _, callback, sid) = (subject, payload, callback, sid)

pubWithReplyCallback :: (Msg -> IO ()) -> SID -> PubOptions -> PubOptions
pubWithReplyCallback callback sid (subject, payload, _, _) = (subject, payload, Just callback, sid)

replyCallback :: NatsConn a => NatsAPI a -> (Msg -> IO ()) -> BS.ByteString -> BS.ByteString -> (Msg -> IO ())
replyCallback nats callback sid subject msg = do
  unsub nats sid subject
  callback msg
