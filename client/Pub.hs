{-# LANGUAGE OverloadedStrings #-}

module Pub where

import qualified Data.ByteString    as BS
import           Nats.Nats
import           Sid
import           Sub
import           Types
import           Types.Msg
import           Types.Pub

type PubOptions = (Subject, BS.ByteString, Maybe (Msg -> IO ()))

applyPubOptions :: PubOptions -> [PubOptions -> PubOptions] -> PubOptions
applyPubOptions = foldl (flip ($))

defaultPubOptions :: PubOptions
defaultPubOptions = ("", "", Nothing)

pub :: NatsConn a => NatsAPI a -> [PubOptions -> PubOptions] -> IO ()
pub nats options = do
  let (subject, payload, callback) = applyPubOptions defaultPubOptions options
  case callback of
    Nothing -> do
      prepareSend nats
      sendBytes nats $ Pub subject Nothing Nothing (Just payload)
    Just cb -> do
      -- replyTo needs to be unique for each message, so many calls can be made
      -- to the same subject with different closures.
      -- For human readability, we include the subject, but this is not necessary.
      -- The sid also isn't really a sid, it's just a valid unique string
      sid <- sidGen
      let replyTo = foldr BS.append "" ["INBOX.", subject, ".", sid]
      sub nats [
        subWithSubject replyTo,
        subWithCallback cb,
        subWithOneOff True
        ]
      prepareSend nats
      sendBytes nats $ Pub subject (Just replyTo) Nothing (Just payload)

pubWithSubject :: Subject -> PubOptions -> PubOptions
pubWithSubject subject (_, payload, callback) = (subject, payload, callback)

pubWithPayload :: BS.ByteString -> PubOptions -> PubOptions
pubWithPayload payload (subject, _, callback) = (subject, payload, callback)

pubWithReplyCallback :: (Msg -> IO ()) -> PubOptions -> PubOptions
pubWithReplyCallback callback (subject, payload, _) = (subject, payload, Just callback)

