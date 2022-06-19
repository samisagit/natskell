{-# LANGUAGE OverloadedStrings #-}

module NatsWrappers where

import           Control.Concurrent
import           Control.Exception
import qualified Data.Text
import qualified Data.Text           as Text
import qualified Docker.Client       as DC
import qualified Docker.Client.Types as DCT
import qualified Network.HTTP        as HTTP
import qualified Network.Simple.TCP  as TCP

second = 1000000

runNATSContainer :: IO (DC.ContainerID, DCT.ContainerDetails)
runNATSContainer = do
  h <- DC.unixHttpHandler "/var/run/docker.sock"
  DC.runDockerT (DC.defaultClientOpts, h) $
    do
      let hpa = DC.HostPort "0.0.0.0" 0
      let pba = DC.PortBinding 4222 DC.TCP [hpa]

      let hpb = DC.HostPort "0.0.0.0" 0
      let pbb = DC.PortBinding 8222 DC.TCP [hpb]

      let createOpts = DC.addPortBinding pba $ DC.addPortBinding pbb $ DC.defaultCreateOpts "nats:latest"
      cid <- DC.createContainer createOpts Nothing
      case cid of
        Left err -> error $ show err
        Right i -> do
          e <- DC.startContainer DC.defaultStartOpts i
          case e of
            Left err -> error $ show err
            Right _  -> do
              details <- DC.inspectContainer i
              case details of
                Left err -> error $ show err
                Right d  -> return (i, d)

ensureNATS :: (DC.ContainerID, DCT.ContainerDetails) -> IO (DC.ContainerID, String, Int)
ensureNATS (id, d) = do
  ensureNATSIsListening hostPortB 10
  return (id, (Text.unpack $ DCT.hostIp hostPortA), (fromIntegral $ DCT.hostPost hostPortA))
  where
    networkPortA = (DCT.networkSettingsPorts $ DCT.networkSettings d) !! 2
    networkPortB = (DCT.networkSettingsPorts $ DCT.networkSettings d) !! 0
    hostPortA = head $ DCT.hostPorts networkPortA
    hostPortB = head $ DCT.hostPorts networkPortB

callNATSHealth hp retryCount = do
  res <- HTTP.simpleHTTP (HTTP.getRequest $ "http://" ++ (Text.unpack $ DC.hostIp hp) ++ ":" ++  (show $ DC.hostPost hp))
  case res of
    Left e -> do
      if retryCount == 0
        then error $ "retried too many times " ++ show e
      else do
        threadDelay $ second `div` 10
        callNATSHealth hp $ retryCount - 1
    Right res -> do
      return $ HTTP.rspCode res

ensureNATSIsListening :: DC.HostPort -> Int -> IO ()
ensureNATSIsListening hp retryCount = do
  result <- try(callNATSHealth hp retryCount) :: IO (Either SomeException HTTP.ResponseCode)
  case result of
    Left ex -> do
      if retryCount == 0
        then error $ "retried too many times " ++ show ex
      else do
        threadDelay $ second `div` 2
        ensureNATSIsListening hp $ retryCount - 1
    Right code -> case code of
      (2, 0, 0) -> return ()
      b         -> do
        if retryCount == 0
          then error $ "retried too many times: status: " ++ show b
        else do
          threadDelay $ second `div` 2
          ensureNATSIsListening hp $ retryCount - 1

startNATS :: IO (DC.ContainerID, String, Int)
startNATS = do
  (id, d) <- runNATSContainer
  ensureNATS (id, d)

stopNATS :: (DC.ContainerID, String, Int) -> IO ()
stopNATS (cid, _, _) = do
  h <- DC.unixHttpHandler "/var/run/docker.sock"
  DC.runDockerT (DC.defaultClientOpts, h) $
    do r <- DC.stopContainer DC.DefaultTimeout cid
       case r of
         Left e  -> error $ show e
         Right _ -> return ()

