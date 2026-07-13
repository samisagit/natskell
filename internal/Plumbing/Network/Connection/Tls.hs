{-# LANGUAGE TypeApplications #-}

module Network.Connection.Tls
  ( configureTransport
  , upgradeTcp
  ) where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as LBS
import           Data.Maybe                 (fromMaybe)
import           Data.X509.CertificateStore (makeCertificateStore)
import           Data.X509.Memory           (readSignedObjectFromMemory)
import           Network.Connection.Core
    ( bufferRead
    , currentTransport
    , enableReadWorker
    , pointTransport
    )
import           Network.Connection.Types
    ( Conn
    , Transport (..)
    , TransportOption (..)
    )
import qualified Network.Socket             as NS
import qualified Network.Socket.ByteString  as NSB
import qualified Network.TLS                as TLS
import           System.X509                (getSystemCertificateStore)
import           Types.TLS                  (TLSConfig (..))

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

upgradeToTLS :: Conn -> TLS.ClientParams -> IO (Either String ())
upgradeToTLS conn params = do
  current <- currentTransport conn
  case current of
    Nothing -> return $ Left "Transport not initialized"
    Just currentTransport' ->
      case transportUpgrade currentTransport' of
        Nothing -> return $ Left "Transport does not support TLS"
        Just upgrade -> do
          result <- upgrade params
          case result of
            Left err -> return $ Left err
            Right newTransport -> do
              pointTransport conn newTransport
              return $ Right ()

upgradeToTLSWithConfig :: Conn -> String -> TLSConfig -> IO (Either String ())
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
      case transportTlsConfig transportOption of
        Nothing -> return (Left "TLS transport requested without TLS configuration")
        Just tlsConfig -> do
          result <- upgradeToTLSWithConfig conn (transportHost transportOption) tlsConfig
          case result of
            Left err -> return (Left err)
            Right () -> do
              enableReadWorker conn
              return (Right ())
    else do
      bufferRead conn (transportInitialBytes transportOption)
      enableReadWorker conn
      return (Right ())

buildTlsParams :: String -> TLSConfig -> IO (Either String TLS.ClientParams)
buildTlsParams host tlsConfig = do
  result <- try @SomeException $ do
    systemStore <- getSystemCertificateStore
    let verificationHost = fromMaybe host (tlsServerName tlsConfig)
        base = TLS.defaultParamsClient verificationHost (BC.pack verificationHost)
        hooks = TLS.clientHooks base
        shared = TLS.clientShared base
        parsedRoots = map readSignedObjectFromMemory (tlsRootCertificates tlsConfig)
        invalidRoot = any null parsedRoots
        caStore = systemStore <> makeCertificateStore (concat parsedRoots)
        verificationHooks =
          if tlsInsecure tlsConfig
            then hooks { TLS.onServerCertificate = \_ _ _ _ -> return [] }
            else hooks
        sharedWithRoots = shared { TLS.sharedCAStore = caStore }
    if invalidRoot
      then fail "TLS root certificate PEM is invalid"
      else
        case tlsClientCertificate tlsConfig of
          Nothing ->
            pure base
              { TLS.clientHooks = verificationHooks
              , TLS.clientShared = sharedWithRoots
              }
          Just (certPem, keyPem) ->
            case TLS.credentialLoadX509FromMemory certPem keyPem of
              Left err -> fail err
              Right cred ->
                pure base
                  { TLS.clientHooks =
                      verificationHooks
                        { TLS.onCertificateRequest = \_ -> return (Just cred)
                        }
                  , TLS.clientShared =
                      sharedWithRoots
                        { TLS.sharedCredentials = TLS.Credentials [cred]
                        }
                  }
  case result of
    Left err ->
      case fromException err :: Maybe SomeAsyncException of
        Just _  -> throwIO err
        Nothing -> pure (Left (displayException err))
    Right params -> pure (Right params)
