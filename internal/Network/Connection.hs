{-# LANGUAGE TypeApplications #-}

module Network.Connection
  ( module Network.Connection.Types
  , newConn
  , readChunkSize
  , tcpTransport
  , openTcpTransport
  , tlsTransport
  , upgradeTcp
  , startReadWorker
  , readWorkerLoop
  , enqueueReadResult
  , enableReadWorker
  , pointTransport
  , close
  , open
  , bufferRead
  , upgradeToTLS
  , upgradeToTLSWithConfig
  , configureTransport
  , buildTlsParams
  , connectionApi
  ) where

import           Control.Concurrent        (forkIO)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           Data.Maybe
import           Network.API
import           Network.Connection.Types
import           Network.ConnectionAPI     (ConnectionAPI (..))
import qualified Network.Simple.TCP        as TCP
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.TLS               as TLS

newConn :: IO Conn
newConn =
  Conn
    <$> newEmptyTMVarIO
    <*> newEmptyTMVarIO
    <*> newEmptyTMVarIO
    <*> newTVarIO mempty
    <*> newTBQueueIO 1000
    <*> newTVarIO False
    <*> newTVarIO False

readChunkSize :: Int
readChunkSize = 4096

tcpTransport :: NS.Socket -> Transport
tcpTransport sock =
  Transport
    { transportRead = NSB.recv sock
    , transportWrite = NSB.sendAll sock
    , transportWriteLazy = NSB.sendMany sock . LBS.toChunks
    , transportFlush = pure ()
    , transportClose = NS.close sock
    , transportUpgrade = Just (upgradeTcp sock)
    }

openTcpTransport :: String -> Int -> IO (Either String Transport)
openTcpTransport host port = do
  result <- try @SomeException $ do
    (sock, _) <- TCP.connectSock host (show port)
    NS.setSocketOption sock NS.NoDelay 1
    NS.setSocketOption sock NS.Cork 0
    pure (tcpTransport sock)
  case result of
    Left err        -> return (Left (show err))
    Right transport -> return (Right transport)

tlsTransport :: TLS.Context -> Transport
tlsTransport ctx =
  Transport
    { transportRead = const (TLS.recvData ctx)
    , transportWrite = TLS.sendData ctx . LBS.fromStrict
    , transportWriteLazy = TLS.sendData ctx
    , transportFlush = TLS.contextFlush ctx
    , transportClose = do
        void $ try @SomeException (TLS.bye ctx)
        void $ try @SomeException (TLS.contextClose ctx)
    , transportUpgrade = Nothing
    }

upgradeTcp :: NS.Socket -> TLS.ClientParams -> IO (Either String Transport)
upgradeTcp sock params = do
  let backend = TLS.Backend
        { TLS.backendSend = NSB.sendAll sock
        , TLS.backendRecv = NSB.recv sock
        , TLS.backendFlush = pure ()
        , TLS.backendClose = NS.close sock
        }
  result <- try @SomeException $ do
    ctx <- TLS.contextNew backend params
    TLS.handshake ctx
    pure (tlsTransport ctx)
  case result of
    Left err        -> return $ Left (show err)
    Right transport -> return $ Right transport

startReadWorker :: Conn -> IO ()
startReadWorker conn = do
  shouldStart <- atomically $ do
    enabled <- readTVar (readWorkerEnabled conn)
    running <- readTVar (readWorkerRunning conn)
    if not enabled || running
      then return False
      else do
        writeTVar (readWorkerRunning conn) True
        return True
  when shouldStart . void $ forkIO (readWorkerLoop conn)

readWorkerLoop :: Conn -> IO ()
readWorkerLoop conn = do
  let cleanup = atomically $ writeTVar (readWorkerRunning conn) False
  finally loop cleanup
  where
    loop = do
      (blocked, enabled) <- atomically $ do
        blocked <- tryReadTMVar (readBlock conn)
        enabled <- readTVar (readWorkerEnabled conn)
        return (blocked, enabled)
      case (blocked, enabled) of
        (Just _, _) -> return ()
        (_, False)  -> return ()
        _ -> do
          current <- atomically $ tryReadTMVar (transport conn)
          case current of
            Nothing -> return ()
            Just currentTransport -> do
              result <- try @SomeException (transportRead currentTransport readChunkSize)
              shouldContinue <- enqueueReadResult conn result
              case result of
                Left _  -> return ()
                Right _ -> when shouldContinue loop

enqueueReadResult :: Conn -> Either SomeException ByteString -> IO Bool
enqueueReadResult conn result = atomically $
  (do
    blocked <- tryReadTMVar (readBlock conn)
    check (isNothing blocked)
    writeTBQueue (readQueue conn) (either (Left . show) Right result)
    return True)
  `orElse`
  (do
    _ <- readTMVar (readBlock conn)
    return False)

enableReadWorker :: Conn -> IO ()
enableReadWorker conn = atomically $ writeTVar (readWorkerEnabled conn) True

instance ConnectionReader Conn where
  readData conn n = do
    blocked <- atomically $ tryReadTMVar (readBlock conn)
    if isJust blocked
      then return $ Left "Read operation is blocked"
      else do
        buffered <- atomically $ do
          buf <- readTVar (readBuffer conn)
          if BS.null buf
            then return Nothing
            else do
              let (chunk, rest) = BS.splitAt n buf
              writeTVar (readBuffer conn) rest
              return (Just chunk)
        case buffered of
          Just chunk -> return $ Right chunk
          Nothing -> do
            mc <- atomically $ tryReadTMVar (transport conn)
            case mc of
              Nothing               -> return $ Left "Transport not initialized"
              Just currentTransport -> do
                enabled <- readTVarIO (readWorkerEnabled conn)
                if enabled
                  then do
                    startReadWorker conn
                    result <- atomically $
                      (Left "Read operation is blocked" <$ readTMVar (readBlock conn))
                      `orElse`
                      readTBQueue (readQueue conn)
                    case result of
                      Left err -> return $ Left err
                      Right bytes -> do
                        let (chunk, rest) = BS.splitAt n bytes
                        unless (BS.null rest)
                          (atomically $ modifyTVar' (readBuffer conn) (<> rest))
                        return $ Right chunk
                  else do
                    resultVar <- newEmptyTMVarIO
                    _ <- forkIO $ do
                      result <- try @SomeException (transportRead currentTransport n)
                      atomically . void $ tryPutTMVar resultVar result
                    result <- atomically $
                      (Left "Read operation is blocked" <$ readTMVar (readBlock conn))
                      `orElse`
                      (Right <$> readTMVar resultVar)
                    case result of
                      Left err -> return $ Left err
                      Right (Left err) -> return $ Left (show err)
                      Right (Right bytes) -> do
                        let (chunk, rest) = BS.splitAt n bytes
                        unless (BS.null rest)
                          (atomically $ modifyTVar' (readBuffer conn) (<> rest))
                        return $ Right chunk

  closeReader conn = do
    void . atomically $ tryPutTMVar (readBlock conn) ()
  openReader conn = void . atomically $ tryTakeTMVar (readBlock conn)

instance ConnectionWriter Conn where
  writeData conn bytes = do
    isBlocked <- atomically $ tryReadTMVar (writeBlock conn)
    if isJust isBlocked
      then return $ Left "Write operation is blocked"
      else do
        current <- atomically $ tryReadTMVar (transport conn)
        case current of
          Nothing -> return $ Left "Transport not initialized"
          Just currentTransport -> do
            result <- try @SomeException $ do
              transportWrite currentTransport bytes
              transportFlush currentTransport
            case result of
              Left err -> return $ Left (show err)
              Right _  -> return $ Right ()

  writeDataLazy conn bytes = do
    isBlocked <- atomically $ tryReadTMVar (writeBlock conn)
    if isJust isBlocked
      then return $ Left "Write operation is blocked"
      else do
        current <- atomically $ tryReadTMVar (transport conn)
        case current of
          Nothing -> return $ Left "Transport not initialized"
          Just currentTransport -> do
            result <- try @SomeException $ do
              transportWriteLazy currentTransport bytes
              transportFlush currentTransport
            case result of
              Left err -> return $ Left (show err)
              Right _  -> return $ Right ()

  closeWriter conn = void . atomically $ tryPutTMVar (writeBlock conn) ()
  openWriter conn = void . atomically $ tryTakeTMVar (writeBlock conn)

instance Connection Conn where
  closeConnection = close
  openConnection = open

pointTransport :: Conn -> Transport -> IO ()
pointTransport conn newTransport = atomically $ do
  _ <- tryTakeTMVar (transport conn)
  putTMVar (transport conn) newTransport

close :: Conn -> IO ()
close conn = do
  closeReader conn
  closeWriter conn
  current <- atomically $ tryReadTMVar (transport conn)
  case current of
    Nothing -> return ()
    Just currentTransport -> void $ try @SomeException (transportClose currentTransport)

open :: Conn -> IO ()
open conn = do
  openReader conn
  openWriter conn
  atomically $ do
    writeTVar (readBuffer conn) mempty
    void $ flushTBQueue (readQueue conn)
    writeTVar (readWorkerEnabled conn) False

bufferRead :: Conn -> ByteString -> IO ()
bufferRead conn bytes =
  unless (BS.null bytes)
    (atomically $ modifyTVar' (readBuffer conn) (bytes <>))

upgradeToTLS :: Conn -> TLS.ClientParams -> IO (Either String ())
upgradeToTLS conn params = do
  current <- atomically $ tryReadTMVar (transport conn)
  case current of
    Nothing -> return $ Left "Transport not initialized"
    Just currentTransport ->
      case transportUpgrade currentTransport of
        Nothing -> return $ Right ()
        Just upgrade -> do
          result <- upgrade params
          case result of
            Left err -> return $ Left err
            Right newTransport -> do
              pointTransport conn newTransport
              return $ Right ()

upgradeToTLSWithConfig :: Conn -> String -> Maybe (ByteString, ByteString) -> IO (Either String ())
upgradeToTLSWithConfig conn host tlsConfig = do
  paramsResult <- buildTlsParams host tlsConfig
  case paramsResult of
    Left err     -> return (Left err)
    Right params -> upgradeToTLS conn params

configureTransport :: Conn -> TransportOption -> IO (Either String ())
configureTransport conn transportOption = do
  let useTls = transportTlsRequested transportOption || transportTlsRequired transportOption
  if useTls
    then do
      result <- upgradeToTLSWithConfig conn (transportHost transportOption) (transportTlsCert transportOption)
      case result of
        Left err -> return (Left err)
        Right () -> do
          enableReadWorker conn
          return (Right ())
    else do
      bufferRead conn (transportInitialBytes transportOption)
      enableReadWorker conn
      return (Right ())

buildTlsParams :: String -> Maybe (ByteString, ByteString) -> IO (Either String TLS.ClientParams)
buildTlsParams host tlsConfig = do
  let base = TLS.defaultParamsClient host (BC.pack host)
      hooks = TLS.clientHooks base
      shared = TLS.clientShared base
      acceptAny = hooks { TLS.onServerCertificate = \_ _ _ _ -> return [] }
  case tlsConfig of
    Just (certPem, keyPem) ->
      case TLS.credentialLoadX509FromMemory certPem keyPem of
        Left err -> return (Left err)
        Right cred ->
          let hooks' = acceptAny { TLS.onCertificateRequest = \_ -> return (Just cred) }
              shared' = shared { TLS.sharedCredentials = TLS.Credentials [cred] }
          in return (Right base { TLS.clientHooks = hooks', TLS.clientShared = shared' })
    Nothing -> return (Right base { TLS.clientHooks = acceptAny })

connectionApi :: ConnectionAPI
connectionApi = ConnectionAPI
  { connectionNew = newConn
  , connectionOpenTcpTransport = openTcpTransport
  , connectionPointTransport = pointTransport
  , connectionConfigure = configureTransport
  }
