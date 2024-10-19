{-# LANGUAGE OverloadedStrings #-}

module NatsWrappers where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString     as BS
import qualified Data.Conduit.Binary as Con
import qualified Data.Text           as Text
import           Debug.Trace
import qualified Docker.Client       as DC
import qualified Docker.Client.Types as DCT
import qualified Network.HTTP        as HTTP

second = 1000000

sock = "/var/run/docker.sock"

startNATS :: Text.Text -> IO (DC.ContainerID, String, Int)
startNATS tag = do
  ensureImage "nats" tag
  (cid, d) <- runNATSContainer tag
  let health = exposedService d 8222
  let nats = exposedService d 4222
  ensureNATS health 10
  syncFile cid
  return (cid, Text.unpack . fst $ nats, snd nats)

stopNATS :: (DC.ContainerID, String, Int) -> IO ()
stopNATS (cid, _, _) = do
  writeLogFile cid
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
  cached <- trace "using cached image" cachedImage image tag
  if cached
  then return ()
  else do
    h <- DC.unixHttpHandler sock
    DC.runDockerT (DC.defaultClientOpts, h) $
      do
        out <- DC.pullImage image tag Con.sinkLbs
        case out of
          Left err -> error $ show err
          Right _  -> return ()

cachedImage :: Text.Text -> Text.Text -> IO Bool
cachedImage image tag = do
  h <- DC.unixHttpHandler sock
  DC.runDockerT (DC.defaultClientOpts, h) $
    do
      out <- DC.listImages undefined
      case out of
        Left err -> error $ show err
        Right images -> do
          let tags = map DC.imageRepoTags images
          let matchString = Text.intercalate (Text.pack ":") [image, tag]
          let match = sum (length . filter (== matchString) <$> tags)
          case match of
            0 -> return False
            _ -> return True

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
        then error $ "retried too many times: " ++ displayException ex
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
exposedService cd containerPort = (DC.hostIp service, fromIntegral . DC.hostPost $ service)
  where
    portBinding = head $ filter (\x -> (fromIntegral . DC.containerPort $ x) == containerPort) (DCT.networkSettingsPorts (DCT.networkSettings cd))
    service = head $ DC.hostPorts portBinding

