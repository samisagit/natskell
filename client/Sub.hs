{-# LANGUAGE OverloadedStrings #-}

module Sub where

import           Client
import           Control.Concurrent
import           Control.Monad
import           Nats.Nats
import           Sid
import           Types
import           Types.Msg
import           Types.Sub
import           Unsub

type SubOptions = (Subject, Msg -> IO(), Bool, Maybe Int)

applySubOptions :: SubOptions -> [SubOptions -> SubOptions] -> SubOptions
applySubOptions = foldl (flip ($))

defaultSubOptions ::  IO SubOptions
defaultSubOptions = do
  return ("", \_ -> return (), False, Nothing)

sub :: NatsConn a => Client a -> [SubOptions -> SubOptions] -> IO SID
sub c@(Client conn _) opts = do
  hashedSid <- sidGen
  defaultOpts <- defaultSubOptions
  let (subject, callback, oneOff, timeout) = applySubOptions defaultOpts opts
  addSubscription conn hashedSid $ \m -> do
    when oneOff $ unsub c hashedSid
    callback m
  sendBytes conn $ Sub subject Nothing hashedSid
  case timeout of
    Just t ->
      void .forkIO $ do
        threadDelay t
        unsub c hashedSid
    Nothing -> return ()
  return hashedSid

subWithSubject :: Subject -> SubOptions -> SubOptions
subWithSubject subject ( _, callback, oneOff, timeout) = (subject, callback, oneOff, timeout)

subWithCallback :: (Msg -> IO()) -> SubOptions -> SubOptions
subWithCallback callback (subject, _, oneOff, timeout) = (subject, callback, oneOff, timeout)

subWithOneOff :: Bool -> SubOptions -> SubOptions
subWithOneOff t (subject, callback, _, timeout) = (subject, callback, t, timeout)

subWithTimeout :: Int -> SubOptions -> SubOptions
subWithTimeout t (subject, callback, oneOff, _) = (subject, callback, oneOff, Just t)

