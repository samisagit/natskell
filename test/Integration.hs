{-# LANGUAGE OverloadedStrings #-}

module Integration where

import qualified Docker.Client as DC

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
           _ <- DC.startContainer DC.defaultStartOpts i
           return i

stopNATSContainer :: DC.ContainerID -> IO ()
stopNATSContainer cid = do
  h <- DC.unixHttpHandler "/var/run/docker.sock"
  DC.runDockerT (DC.defaultClientOpts, h) $
    do r <- DC.stopContainer DC.DefaultTimeout cid
       case r of
         Left e  -> error $ show e
         Right _ -> return ()
