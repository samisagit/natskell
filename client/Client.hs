{-# LANGUAGE OverloadedStrings #-}

module Client (module Sub, module Pub, module Unsub, handShake, connect, Msg (..)) where

import           Control.Concurrent
import           Control.Monad
import           Nats.Nats
import           Nats.NatsProper
import           Parsers.Parsers
import           Pub
import           Sub
import           Types.Connect
import           Types.Info
import           Types.Msg
import           Types.Ping
import           Types.Pong
import           Types.Ok
import           Types.Err
import           Unsub

-- TODO: this name is a bit misleading, it doesn't just perform the handshake
-- it also starts the read and write threads
handShake :: NatsConn a => NatsAPI a -> IO ()
handShake nats = do
  forkIO . forever $ recvBytes nats
  forkIO . forever $ readMessages nats
  -- TODO: at some point we'll want to read the state set by INFO
  prepareSend nats
  sendBytes nats $
    Connect
      { Types.Connect.verbose = True,
        Types.Connect.pedantic = True,
        Types.Connect.tls_required = False,
        Types.Connect.auth_token = Nothing,
        Types.Connect.user = Nothing,
        Types.Connect.pass = Nothing,
        Types.Connect.name = Nothing,
        Types.Connect.lang = "Haskell",
        Types.Connect.version = "2010",
        Types.Connect.protocol = Nothing,
        Types.Connect.echo = Nothing,
        Types.Connect.sig = Nothing,
        Types.Connect.jwt = Nothing,
        Types.Connect.no_responders = Nothing,
        Types.Connect.headers = Nothing
      }

pong :: NatsConn a => NatsAPI a -> IO ()
pong nats = sendBytes nats Pong

readMessages :: NatsConn a => NatsAPI a -> IO ()
readMessages nats = do
  msg <- readMessage nats
  case msg of
    ParsedMsg a     -> void . forkIO $ handleMsg nats a
    ParsedPing Ping -> void . forkIO $ pong nats
    ParsedInfo a    -> void . forkIO $ handleInfo nats a
    ParsedOk a      -> handleOk nats a
    ParsedPong _    -> return ()
    -- TODO: we should check the error to see if it' fatal, if so we'll have been disconnected
    ParsedErr err   -> handleErr nats err

handleErr :: NatsConn a => NatsAPI a -> Err -> IO ()
handleErr nats err = do 
  ackBytes nats
  void . forkIO . print $ "error: " ++ show err

-- TODO: if safe mode, unblock further sends
handleOk :: NatsConn a => NatsAPI a -> Ok -> IO ()
handleOk nats _ = ackBytes nats

handleMsg :: NatsConn a => NatsAPI a -> Msg -> IO ()
handleMsg nats msg = do
  f <- subscriptionCallback nats (Types.Msg.sid msg)
  f msg

handleInfo :: NatsConn a => NatsAPI a -> Info -> IO ()
handleInfo nats info =
  setConfig
    nats
    Config
      { maxPayload = max_payload info,
        headersSupported = isTruthy (Types.Info.headers info),
        authRequired = isTruthy (Types.Info.auth_required info),
        _TLSRequired = isTruthy (Types.Info.tls_required info),
        connectURLs = connect_urls info,
        safeMode = True -- TODO: this should be set by the user
      }

isTruthy :: Maybe Bool -> Bool
isTruthy Nothing  = False
isTruthy (Just b) = b

