{-# LANGUAGE TypeApplications #-}

module Network.Connection where

import           Control.Concurrent        (ThreadId, myThreadId, throwTo)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as BSL
import           Data.Maybe
import           Network.API
import qualified Network.Simple.TCP        as TCP
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.TLS               as TLS

data Transport = Transport
                   { transportRead :: Int -> IO ByteString
                   , transportWrite :: ByteString -> IO ()
                   , transportFlush :: IO ()
                   , transportClose :: IO ()
                   , transportUpgrade :: Maybe (TLS.ClientParams -> IO (Either String Transport))
                   }

data TransportOptions = TransportOptions
                          { transportHost :: String
                          , transportTlsRequired :: Bool
                          , transportTlsRequested :: Bool
                          , transportTlsCert :: Maybe (ByteString, ByteString)
                          , transportInitialBytes :: ByteString
                          }

data Conn = Conn
              { transport  :: TMVar Transport
              , readBlock  :: TMVar ()
              , writeBlock :: TMVar ()
              , readBuffer :: TVar ByteString
              , readThread :: TVar (Maybe ThreadId)
              }

newConn :: IO Conn
newConn =
  Conn <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newTVarIO mempty <*> newTVarIO Nothing

data ReadAbort = ReadAbort
  deriving (Show)

instance Exception ReadAbort

tcpTransport :: NS.Socket -> Transport
tcpTransport sock =
  Transport
    { transportRead = NSB.recv sock
    , transportWrite = NSB.sendAll sock
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
    NS.setSocketOption sock NS.RecvBuffer 1
    NS.setSocketOption sock NS.SendBuffer 1
    pure (tcpTransport sock)
  case result of
    Left err        -> return (Left (show err))
    Right transport -> return (Right transport)

tlsTransport :: TLS.Context -> Transport
tlsTransport ctx =
  Transport
    { transportRead = const (TLS.recvData ctx)
    , transportWrite = TLS.sendData ctx . BSL.fromStrict
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
              Just currentTransport -> readFromTransport currentTransport
    where
      readFromTransport currentTransport = do
        tid <- myThreadId
        shouldRead <- atomically $ do
          blocked <- tryReadTMVar (readBlock conn)
          case blocked of
            Just _  -> return False
            Nothing -> do
              writeTVar (readThread conn) (Just tid)
              return True
        if not shouldRead
          then return $ Left "Read operation is blocked"
          else do
            let clearThread = atomically $ writeTVar (readThread conn) Nothing
            result <- try @SomeException (transportRead currentTransport n) `finally` clearThread
            case result of
              Left err ->
                case fromException err of
                  Just ReadAbort -> return $ Left "Read aborted due to block"
                  Nothing        -> return $ Left (show err)
              Right bytes -> do
                let (chunk, rest) = BS.splitAt n bytes
                unless (BS.null rest)
                  (atomically $ modifyTVar' (readBuffer conn) (<> rest))
                return $ Right chunk

  closeReader conn = do
    void . atomically $ tryPutTMVar (readBlock conn) ()
    mtid <- readTVarIO (readThread conn)
    maybe (return ()) (`throwTo` ReadAbort) mtid
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
  atomically $ writeTVar (readBuffer conn) mempty

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

configureTransport :: Conn -> TransportOptions -> IO (Either String ())
configureTransport conn options = do
  let useTls = transportTlsRequested options || transportTlsRequired options
  if useTls
    then upgradeToTLSWithConfig conn (transportHost options) (transportTlsCert options)
    else do
      bufferRead conn (transportInitialBytes options)
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
