{-# LANGUAGE OverloadedStrings #-}

module Pub where

import qualified Data.ByteString    as BS
import           Types.Pub
import           Types.Msg
import           Nats.Nats
import Sub
import Unsub
import Types

type PubOptions = (Subject, BS.ByteString, Maybe (Msg -> IO ()), SID)

applyPubOptions :: PubOptions -> [PubOptions -> PubOptions] -> PubOptions
applyPubOptions = foldl (flip ($))

defaultPubOptions :: PubOptions
defaultPubOptions = ("", "", Nothing, "")

-- TODO: we'll want to gc reply subs that haven't been recieved in a given duration
-- we could plausibly add a timeout when adding a subscription, and periodically check
-- for expired subs
pub :: NatsConn a => NatsAPI a -> [PubOptions -> PubOptions] -> IO ()
pub nats options = do
  let (subject, payload, callback, sid) = applyPubOptions defaultPubOptions options
  case callback of
    Nothing -> sendBytes nats $ Pub subject Nothing Nothing (Just payload)
    Just cb -> do
      let replyTo = BS.append subject ".REPLY" -- TODO: this subject needs to be unique, so user created subs don't get removed on reciept
      sub nats [ 
        subWithSID sid, -- TODO: what should this be
        subWithSubject replyTo,
        subWithCallback (replyCallback nats cb sid subject),
        subWithTimeout 10
        ]
      sendBytes nats $ Pub subject (Just replyTo) Nothing (Just payload)

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
