{-# LANGUAGE OverloadedStrings #-}

module Sub where

import           Types.Sub
import           Types.Msg
import           Nats.Nats
import           Types

type SubOptions = (Subject, SID, Msg -> IO(), Maybe Int)

applySubOptions :: SubOptions -> [SubOptions -> SubOptions] -> SubOptions
applySubOptions = foldl (flip ($))

defaultSubOptions :: SubOptions
defaultSubOptions = ("", "", \_ -> return (), Nothing)

sub :: NatsConn a => NatsAPI a -> [SubOptions -> SubOptions] -> IO ()
sub nats opts = do
  let (subject, sid, callback, timeout) = applySubOptions defaultSubOptions opts
  addSubscription nats sid callback timeout
  sendBytes nats $ Sub subject Nothing sid

subWithSubject :: Subject -> SubOptions -> SubOptions
subWithSubject subject ( _, sid, callback, timeout) = (subject, sid, callback, timeout)

subWithSID :: SID -> SubOptions -> SubOptions
subWithSID sid (subject, _, callback, timeout) = (subject, sid, callback, timeout)

subWithTimeout :: Int -> SubOptions -> SubOptions
subWithTimeout timeout (subject, sid, callback, _) = (subject, sid, callback, Just timeout)

subWithCallback :: (Msg -> IO()) -> SubOptions -> SubOptions
subWithCallback callback (subject, sid, _, timeout) = (subject, sid, callback, timeout)
