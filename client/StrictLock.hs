{-# LANGUAGE GADTs #-}
module StrictLock where

import           Control.Concurrent

data StrictLock where
  StrictLock :: {
    requestLock :: MVar (),
    connLock :: MVar ()
    } -> StrictLock

newStrictLock :: IO StrictLock
newStrictLock = do
  requestLock' <- newMVar ()
  connLock' <- newEmptyMVar

  return StrictLock {
  requestLock = requestLock',
  connLock = connLock'
}

request :: Maybe StrictLock -> IO() -> IO()
request (Just sl) action = do
  takeMVar (requestLock sl) -- block an futher actions until acked
  action
  takeMVar (connLock sl) -- block this request from resolving until acked
request Nothing action = do
  action

ack :: Maybe StrictLock -> IO ()
ack (Just sl) = do
  putMVar (requestLock sl) ()
  putMVar (connLock sl) ()
ack Nothing   = return ()
