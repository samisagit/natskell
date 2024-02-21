{-# LANGUAGE OverloadedStrings #-}

module Sub where

import           Nats.Nats
import           Sid
import           Types
import           Types.Msg
import           Types.Sub

type SubOptions = (Subject, Msg -> IO())

applySubOptions :: SubOptions -> [SubOptions -> SubOptions] -> SubOptions
applySubOptions = foldl (flip ($))

defaultSubOptions ::  IO SubOptions
defaultSubOptions = do
  return ("", \_ -> return ())

sub :: NatsConn a => NatsAPI a -> [SubOptions -> SubOptions] -> IO SID
sub nats opts = do
  hashedSid <- sidGen
  defaultOpts <- defaultSubOptions
  let (subject, callback) = applySubOptions defaultOpts opts
  addSubscription nats hashedSid callback
  prepareSend nats
  sendBytes nats $ Sub subject Nothing hashedSid
  return hashedSid

subWithSubject :: Subject -> SubOptions -> SubOptions
subWithSubject subject ( _, callback) = (subject, callback)

subWithCallback :: (Msg -> IO()) -> SubOptions -> SubOptions
subWithCallback callback (subject, _) = (subject, callback)

