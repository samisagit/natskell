{-# LANGUAGE OverloadedStrings #-}

module Sub where

import           Nats.Nats
import           Types
import           Types.Msg
import           Types.Sub

type SubOptions = (Subject, SID, Msg -> IO())

applySubOptions :: SubOptions -> [SubOptions -> SubOptions] -> SubOptions
applySubOptions = foldl (flip ($))

defaultSubOptions :: SubOptions
defaultSubOptions = ("", "", \_ -> return ())

sub :: NatsConn a => NatsAPI a -> [SubOptions -> SubOptions] -> IO ()
sub nats opts = do
  let (subject, sid, callback) = applySubOptions defaultSubOptions opts
  addSubscription nats sid callback
  sendBytes nats $ Sub subject Nothing sid

subWithSubject :: Subject -> SubOptions -> SubOptions
subWithSubject subject ( _, sid, callback) = (subject, sid, callback)

subWithSID :: SID -> SubOptions -> SubOptions
subWithSID sid (subject, _, callback) = (subject, sid, callback)

subWithCallback :: (Msg -> IO()) -> SubOptions -> SubOptions
subWithCallback callback (subject, sid, _) = (subject, sid, callback)
