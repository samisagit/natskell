{-# LANGUAGE GADTs #-}
module StrictLock where

import           Control.Concurrent

data StrictLock where
  StrictLock :: {requestLock :: MVar ()} -> StrictLock

newStrictLock :: IO StrictLock
newStrictLock = do
  requestLock' <- newMVar ()

  return StrictLock {
  requestLock = requestLock'
}

request :: Maybe StrictLock -> IO() -> IO()
request (Just sl) action = do
  takeMVar (requestLock sl)
  action
request Nothing action = do
  action

ack :: Maybe StrictLock -> IO ()
ack (Just sl) = putMVar (requestLock sl) ()
ack Nothing   = return ()
