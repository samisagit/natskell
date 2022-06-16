{-# LANGUAGE OverloadedStrings #-}

module Integration where

import           Control.Concurrent
import           Control.Exception
import qualified Docker.Client      as DC
import qualified Network.Simple.TCP as TCP

runNATSContainer :: IO DC.ContainerID
runNATSContainer = do
  h <- DC.unixHttpHandler "/var/run/docker.sock"
  DC.runDockerT (DC.defaultClientOpts, h) $
    do let pb = DC.PortBinding 4222 DC.TCP [DC.HostPort "0.0.0.0" 4222]
       let createOpts = DC.addPortBinding pb $ DC.defaultCreateOpts "nats:latest"
       cid <- DC.createContainer createOpts Nothing
       case cid of
         Left err -> error $ show err
         Right i -> do
           e <- DC.startContainer DC.defaultStartOpts i
           case e of
             Left err -> error $ show err
             Right _  -> do
               return i

ensureNATS :: DC.ContainerID -> IO DC.ContainerID
ensureNATS id = do
  _ <- ensureNATSIsListening 10
  return id

ensureNATSIsListening :: Int -> IO ()
ensureNATSIsListening retryCount = do
  result <- try(TCP.connect "0.0.0.0" "4222" $ \(sock, _) -> do
    TCP.closeSock sock
    ) :: IO (Either SomeException ())
  case result of
    Left ex -> do
      print $ show ex
      if retryCount == 0
        then error $ "retried too many times " ++ show ex
      else do
        threadDelay $ 1000000
        ensureNATSIsListening $ retryCount - 1
    Right _ -> ensureNATSIsResponding retryCount

ensureNATSIsResponding :: Int -> IO ()
ensureNATSIsResponding retryCount = do
  result <- try(TCP.connect "0.0.0.0" "4222" $ \(sock, _) -> do
    res <- TCP.recv sock 1000
    TCP.closeSock sock
    case res of
      Just a  -> return ()
      Nothing -> do
        if retryCount == 0
          then error $ "retried too many times (no response)"
        else do
          ensureNATSIsResponding $ retryCount-1
    ) :: IO (Either SomeException ())
  case result of
    Left ex -> do
      if retryCount == 0
        then error $ "retried too many times " ++ show ex
      else do
        ensureNATSIsResponding $ retryCount-1
    Right _ -> return ()

startNATS :: IO DC.ContainerID
startNATS = do
  id <- runNATSContainer
  ensureNATS id

stopNATS :: DC.ContainerID -> IO ()
stopNATS cid = do
  h <- DC.unixHttpHandler "/var/run/docker.sock"
  DC.runDockerT (DC.defaultClientOpts, h) $
    do r <- DC.stopContainer DC.DefaultTimeout cid
       case r of
         Left e  -> error $ show e
         Right _ -> return ()

