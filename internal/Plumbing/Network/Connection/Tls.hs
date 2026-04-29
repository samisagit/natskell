{-# LANGUAGE TypeApplications #-}

module Network.Connection.Tls
  ( configureTransport
  , upgradeTcp
  ) where

import           Control.Exception
import           Control.Monad
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
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
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.TLS               as TLS

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
