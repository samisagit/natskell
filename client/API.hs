{-# LANGUAGE OverloadedStrings #-}

module API (connect, newClient, Client, withAckCallback, handShake, module Sub, module Pub, module Unsub, Msg (..)) where

import           Client
import           Control.Concurrent
import           Control.Monad
import           Nats.Nats
import           Nats.NatsProper
import           Parsers.Parsers
import           Pub
import           Sub
import           Types.Connect
import           Types.Err
import           Types.Info
import           Types.Msg
import           Types.Ping
import           Types.Pong
import           Unsub

-- TODO: this name is a bit misleading, it doesn't just perform the handshake
-- it also starts the read and write threads
handShake :: NatsConn a => Client a -> IO ()
handShake c@(Client conn _) = do
  -- TODO: at some point we'll want to read the state set by INFO
  forkIO . forever $ recvBytes conn
  forkIO . forever $ readMessages c

  sendBytes conn
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
        Types.Connect.headers = Just True
      }

pong :: NatsConn a => Client a -> IO ()
pong (Client conn _) = sendBytes conn Pong

readMessages :: NatsConn a => Client a -> IO ()
readMessages c@(Client conn ack) = do
  msg <- readMessage conn
  case msg of
    ParsedMsg a     -> void . forkIO $ handleMsg c a
    ParsedPing Ping -> void . forkIO $ pong c
    ParsedInfo a    -> void . forkIO $ handleInfo c a
    ParsedOk _      -> handleOk ack
    ParsedPong _    -> return ()
    ParsedErr err   -> handleErr c err

-- TODO: we should check the error to see if it' fatal, if so we'll have been disconnected
handleErr :: NatsConn a => Client a -> Err -> IO ()
handleErr _ err = do
  void . forkIO . print $ "error: " ++ show err

-- if safe mode, unblock further sends
handleOk :: Maybe (IO()) -> IO ()
handleOk (Just ack) = ack
handleOk Nothing    = return ()

handleMsg :: NatsConn a => Client a -> Msg -> IO ()
handleMsg (Client conn _) msg = do
  f <- subscriptionCallback conn (Types.Msg.sid msg)
  f msg

handleInfo :: NatsConn a => Client a -> Info -> IO ()
handleInfo (Client conn _) info = do
  setConfig
    conn
    Config
      { maxPayload = max_payload info,
        headersSupported = isTruthy (Types.Info.headers info),
        authRequired = isTruthy (Types.Info.auth_required info),
        _TLSRequired = isTruthy (Types.Info.tls_required info),
        connectURLs = connect_urls info
      }

isTruthy :: Maybe Bool -> Bool
isTruthy Nothing  = False
isTruthy (Just b) = b

