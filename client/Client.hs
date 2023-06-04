{-# LANGUAGE OverloadedStrings #-}

module Client(handShake, connect, pub, sub, pubWithReply, unsub, Msg(..)) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString    as BS
import           Nats.Nats
import           Nats.NatsProper
import           Parsers.Parsers
import           Types.Connect
import           Types.Info
import           Types.Msg
import           Types.Ping
import           Types.Pong
import           Types.Pub
import           Types.Sub

-- TODO: this name is a bit misleading, it doesn't just perform the handshake
-- it also starts the read and write threads
handShake :: NatsConn a => NatsAPI a -> IO ()
handShake nats = do
  forkIO . forever $ recvBytes nats
  forkIO . forever $ readMessages nats
  -- TODO: at some point we'll want to read the state set by INFO
  sendBytes nats $ Connect {
    Types.Connect.verbose       = False,
    Types.Connect.pedantic      = True,
    Types.Connect.tls_required  = False,
    Types.Connect.auth_token    = Nothing,
    Types.Connect.user          = Nothing,
    Types.Connect.pass          = Nothing,
    Types.Connect.name          = Nothing,
    Types.Connect.lang          = "Haskell",
    Types.Connect.version       = "2010",
    Types.Connect.protocol      = Nothing,
    Types.Connect.echo          = Nothing,
    Types.Connect.sig           = Nothing,
    Types.Connect.jwt           = Nothing,
    Types.Connect.no_responders = Nothing,
    Types.Connect.headers       = Nothing
  }

pong :: NatsConn a => NatsAPI a -> IO ()
pong nats = sendBytes nats Pong

pub :: NatsConn a => NatsAPI a -> BS.ByteString -> BS.ByteString -> IO ()
pub nats subject payload = sendBytes nats $ Pub subject Nothing Nothing (Just payload)

sub :: NatsConn a => NatsAPI a -> BS.ByteString -> BS.ByteString -> (Msg -> IO()) -> IO ()
sub nats sid subject callback = do
  addSubscription nats sid callback
  sendBytes nats $ Sub subject Nothing sid

-- TODO: we'll want to gc reply subs that haven't been recieved in a given duration
-- we could plausibly add a timeout when adding a subscription, and periodically check
-- for expired subs
pubWithReply :: NatsConn a => NatsAPI a -> BS.ByteString -> BS.ByteString -> BS.ByteString -> (Msg -> IO()) -> IO ()
pubWithReply nats sid subject payload callback = do
  sub nats sid "SOMETHING.REPLY" cb -- TODO: this subject needs to be unique, so user created subs don't get removed on reciept
  pub nats subject payload
  where
    cb = replyCallback nats callback sid subject

replyCallback :: NatsConn a => NatsAPI a -> (Msg -> IO()) -> BS.ByteString -> BS.ByteString -> (Msg -> IO())
replyCallback nats callback sid subject msg = do
  unsub nats sid subject
  callback msg

unsub :: NatsConn a => NatsAPI a -> BS.ByteString -> BS.ByteString -> IO ()
unsub nats sid subject = do
  removeSubscription nats sid
  sendBytes nats $ Sub subject Nothing sid

readMessages :: NatsConn a => NatsAPI a -> IO ()
readMessages nats = do
  msg <- readMessage nats
  case msg of
    ParsedMsg a     -> void . forkIO $ handleMsg nats a
    ParsedPing Ping -> void . forkIO $ pong nats
    ParsedInfo a    -> void . forkIO $ handleInfo nats a
    ParsedOk _      -> return ()
    ParsedPong _    -> return ()
    -- TODO: we should check the error to see if it' fatal, if so we'll have been disconnected
    ParsedErr err   -> void . forkIO . print $ "error: " ++ show err

handleMsg :: NatsConn a => NatsAPI a -> Msg -> IO ()
handleMsg nats msg = do
  f <- subscriptionCallback nats (Types.Msg.sid msg)
  f msg

handleInfo :: NatsConn a =>  NatsAPI a -> Info -> IO ()
handleInfo nats info = setConfig nats Config {
  maxPayload       = max_payload info,
  headersSupported = isTruthy (Types.Info.headers info),
  authRequired     = isTruthy (Types.Info.auth_required info),
  _TLSRequired     = isTruthy (Types.Info.tls_required info),
  connectURLs      = connect_urls info
  }

isTruthy :: Maybe Bool -> Bool
isTruthy Nothing  = False
isTruthy (Just b) = b

