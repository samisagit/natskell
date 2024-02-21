{-# LANGUAGE OverloadedStrings #-}

module NatsWrappers where

import           Control.Concurrent
import           Control.Monad
import           Control.Exception
import qualified Data.ByteString     as BS
import qualified Data.Conduit.Binary as Con
import qualified Data.Text           as Text
import qualified Docker.Client       as DC
import qualified Docker.Client.Types as DCT
import qualified Network.HTTP        as HTTP

second = 1000000

sock = "/var/run/docker.sock"

startNATS :: Text.Text -> IO (DC.ContainerID, String, Int)
startNATS tag = do
  ensureImage "nats" tag
  (cid, d) <- runNATSContainer tag
  let health = exposedService d 0
  let nats = exposedService d 2
  ensureNATS health 10
  syncFile cid
  return (cid, Text.unpack . fst $ nats, snd nats)

stopNATS :: (DC.ContainerID, String, Int) -> IO ()
stopNATS (cid, _, _) = do
  h <- DC.unixHttpHandler sock
  DC.runDockerT (DC.defaultClientOpts, h) $
    do
      r <- DC.stopContainer DC.DefaultTimeout cid
      case r of
        Left e -> error $ show e
        Right _ -> do
          return ()

writeLogFile :: DC.ContainerID -> IO ()
writeLogFile cid = do
  h <- DC.unixHttpHandler sock
  output <- DC.runDockerT (DC.defaultClientOpts, h) $
    do
      logs <- DC.getContainerLogs DCT.defaultLogOpts cid
      case logs of
        Left e -> error $ show e
        Right l -> do
          return $ BS.toStrict l
  BS.writeFile "nats.log" output

syncFile :: DC.ContainerID -> IO ()
syncFile cid = do
  void . forkIO . forever $ do
    writeLogFile cid
    threadDelay 5000000

ensureImage :: Text.Text -> Text.Text -> IO ()
ensureImage image tag = do
  h <- DC.unixHttpHandler sock
  DC.runDockerT (DC.defaultClientOpts, h) $
    do
      out <- DC.pullImage image tag Con.sinkLbs
      case out of
        Left err -> error $ show err
        Right _  -> return ()

runNATSContainer :: Text.Text -> IO (DC.ContainerID, DCT.ContainerDetails)
runNATSContainer tag = do
  cid <- createNATSContainer tag
  case cid of
    Left err -> error $ show err
    Right i -> do
      startNATSContainer i

createNATSContainer tag = do
  h <- DC.unixHttpHandler sock
  DC.runDockerT (DC.defaultClientOpts, h) $
    do
      let hpa = DC.HostPort "0.0.0.0" 0
      let pba = DC.PortBinding 4222 DC.TCP [hpa]
      let hpb = DC.HostPort "0.0.0.0" 0
      let pbb = DC.PortBinding 8222 DC.TCP [hpb]
      let createOpts = (DC.addPortBinding pba . DC.addPortBinding pbb) . DC.defaultCreateOpts . Text.append "nats:" $ tag
      let containerOpts = (DC.containerConfig createOpts) {DC.cmd = ["-DV", "--config", "nats-server.conf"]}
      let createOpts' = createOpts {DC.containerConfig = containerOpts}
      DC.createContainer createOpts' Nothing

startNATSContainer i = do
  h <- DC.unixHttpHandler sock
  DC.runDockerT (DC.defaultClientOpts, h) $
    do
      e <- DC.startContainer DC.defaultStartOpts i
      case e of
        Left err -> error $ show err
        Right _ -> do
          details <- DC.inspectContainer i
          case details of
            Left err -> error $ show err
            Right d  -> return (i, d)

callNATSHealth :: String -> Int -> IO HTTP.ResponseCode
callNATSHealth host port = do
  res <- HTTP.simpleHTTP (HTTP.getRequest $ "http://" ++ host ++ ":" ++ show port)
  case res of
    Left e -> do
      error $ show e
    Right res -> do
      return $ HTTP.rspCode res

ensureNATS :: (Text.Text, Int) -> Int -> IO ()
ensureNATS service retryCount = do
  result <-
    try
      ( callNATSHealth (Text.unpack . fst $ service) (snd service)
      ) ::
      IO (Either SomeException HTTP.ResponseCode)
  case result of
    Left ex -> do
      if retryCount == 0
        then error $ "retried too many times " ++ show ex
        else do
          threadDelay $ second `div` 2
          ensureNATS service $ retryCount - 1
    Right code -> case code of
      (2, 0, 0) -> return ()
      b -> do
        if retryCount == 0
          then error $ "retried too many times: status: " ++ show b
          else do
            threadDelay $ second `div` 2
            ensureNATS service $ retryCount - 1

exposedService :: DCT.ContainerDetails -> Int -> (Text.Text, Int)
exposedService cd n = (DC.hostIp service, fromIntegral . DC.hostPost $ service)
  where
    networkPortA = DCT.networkSettingsPorts (DCT.networkSettings cd) !! n
    service = head $ DCT.hostPorts networkPortA
