{-# LANGUAGE OverloadedStrings #-}

module Pub (pub, pubWithSubject, pubWithPayload, pubWithReplyCallback, pubWithHeaders) where

import           Client
import qualified Data.ByteString as BS
import           Nats.Nats
import           Sid
import           Sub
import           Types
import           Types.Msg
import           Types.Pub

type PubOptions = (Subject, Payload, Maybe (Msg -> IO ()), Maybe Headers)

applyPubOptions :: PubOptions -> [PubOptions -> PubOptions] -> PubOptions
applyPubOptions = foldl (flip ($))

defaultPubOptions :: PubOptions
defaultPubOptions = ("", "", Nothing, Nothing)

pub :: NatsConn a => Client a -> [PubOptions -> PubOptions] -> IO ()
pub c@(Client conn _) options = do
  let (subject, payload, callback, headers) = applyPubOptions defaultPubOptions options
  case callback of
    Nothing -> do
      sendBytes conn $ Pub subject Nothing headers (Just payload)
    Just cb -> do
      -- replyTo needs to be unique for each message, so many calls can be made
      -- to the same subject with different closures.
      -- For human readability, we include the subject, but this is not necessary.
      -- The sid also isn't really a sid, it's just a valid unique string
      sid <- sidGen
      let replyTo = foldr BS.append "" ["INBOX.", subject, ".", sid]
      sub c [
        subWithSubject replyTo,
        subWithCallback cb,
        subWithOneOff True
        ]
      sendBytes conn $ Pub subject (Just replyTo) Nothing (Just payload)

pubWithSubject :: Subject -> PubOptions -> PubOptions
pubWithSubject subject (_, payload, callback, headers) = (subject, payload, callback, headers)

pubWithPayload :: BS.ByteString -> PubOptions -> PubOptions
pubWithPayload payload (subject, _, callback, headers) = (subject, payload, callback, headers)

pubWithReplyCallback :: (Msg -> IO ()) -> PubOptions -> PubOptions
pubWithReplyCallback callback (subject, payload, _, headers) = (subject, payload, Just callback, headers)

pubWithHeaders :: Headers -> PubOptions -> PubOptions
pubWithHeaders headers (subject, payload, callback, _) = (subject, payload, callback, Just headers)
