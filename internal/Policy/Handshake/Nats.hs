module Handshake.Nats
  ( HandshakeError (..)
  , performHandshake
  ) where

import           Auth.Types
import qualified Data.ByteString           as BS
import           Data.Maybe                (fromMaybe, isJust)
import           Network.ConnectionAPI
    ( Conn
    , ConnectionAPI
    , TransportOption (..)
    , configure
    , readData
    , reader
    , writeDataLazy
    , writer
    )
import           Parser.API
    ( ParseStep (DropPrefix, Emit, NeedMore, Reject)
    , ParsedMessage (..)
    , ParserAPI
    , parse
    )
import           State.Store
    ( ClientState
    , config
    , setServerInfo
    , updateLogContextFromInfo
    )
import           State.Types               (ClientConfig (..))
import           System.Timeout            (timeout)
import           Transformers.Transformers (Transformer (transform))
import qualified Types.Connect             as Connect
import qualified Types.Err                 as Err
import qualified Types.Info                as I
import           Types.Ping                (Ping (Ping))
import           Types.Pong                (Pong (Pong))

data HandshakeError = HandshakeTransportError String
                    | HandshakeProtocolError String
                    | HandshakeAuthError AuthError
                    | HandshakeTimeout
  deriving (Eq, Show)

performHandshake :: ConnectionAPI -> ParserAPI ParsedMessage -> ClientState -> Auth -> Conn -> String -> IO (Either HandshakeError ())
performHandshake connectionApi parserApi state auth conn host = do
  result <- timeout (max 1 (connectTimeoutMicros (config state))) handshake
  pure (fromMaybe (Left HandshakeTimeout) result)
  where
    handshake = do
      infoResult <- readInitialInfo
      case infoResult of
        Left err ->
          pure (Left err)
        Right (info, rest) -> do
          let cfg = config state
              tlsRequested = isJust (tlsCert cfg) || Connect.tls_required (connectConfig cfg)
              tlsRequired = fromMaybe False (I.tls_required info)
              transportOption =
                TransportOption
                  { transportHost = host
                  , transportTlsRequired = tlsRequired
                  , transportTlsRequested = tlsRequested
                  , transportTlsCert = tlsCert cfg
                  , transportInitialBytes = rest
                  }
          transportResult <- configure connectionApi conn transportOption
          case transportResult of
            Left err ->
              pure (Left (HandshakeTransportError err))
            Right () -> do
              setServerInfo state info
              updateLogContextFromInfo state info
              let authContext = AuthContext { authNonce = I.nonce info }
                  connectPayload =
                    (connectConfig cfg)
                      { Connect.tls_required = tlsRequired || tlsRequested
                      }
              case validateAuth auth of
                Left err ->
                  pure (Left (HandshakeAuthError err))
                Right () ->
                  case buildAuthPatch auth authContext of
                    Left err ->
                      pure (Left (HandshakeAuthError err))
                    Right patch -> do
                      writeResult <-
                        writeDataLazy
                          (writer connectionApi)
                          conn
                          ( transform (applyAuthPatch patch connectPayload)
                              <> transform Ping
                          )
                      case writeResult of
                        Left err ->
                          pure (Left (HandshakeTransportError err))
                        Right () ->
                          awaitPong mempty

    readInitialInfo :: IO (Either HandshakeError (I.Info, BS.ByteString))
    readInitialInfo = go mempty
      where
        go acc = do
          result <- readData (reader connectionApi) conn 4096
          case result of
            Left err ->
              pure (Left (HandshakeTransportError err))
            Right chunk ->
              if BS.null chunk
                then pure (Left (HandshakeTransportError "read returned empty chunk before INFO"))
                else do
                  let bytes = acc <> chunk
                  case parse parserApi bytes of
                    NeedMore ->
                      go bytes
                    DropPrefix n _ ->
                      go (BS.drop n bytes)
                    Reject reason ->
                      pure (Left (HandshakeProtocolError reason))
                    Emit (ParsedInfo info) rest ->
                      pure (Right (info, rest))
                    Emit (ParsedErr err) _
                      | isAuthenticationError err ->
                          pure (Left (HandshakeAuthError (AuthError (show err))))
                      | otherwise ->
                          pure (Left (HandshakeProtocolError ("server error before INFO: " ++ show err)))
                    Emit _ rest ->
                      go rest

    awaitPong :: BS.ByteString -> IO (Either HandshakeError ())
    awaitPong acc = do
      result <- readData (reader connectionApi) conn 4096
      case result of
        Left err ->
          pure (Left (HandshakeTransportError err))
        Right chunk
          | BS.null chunk ->
              pure (Left (HandshakeTransportError "read returned empty chunk before PONG"))
          | otherwise ->
              consumePongFrames (acc <> chunk)

    consumePongFrames :: BS.ByteString -> IO (Either HandshakeError ())
    consumePongFrames bytes =
      case parse parserApi bytes of
        NeedMore ->
          awaitPong bytes
        DropPrefix _ reason ->
          pure (Left (HandshakeProtocolError reason))
        Reject reason ->
          pure (Left (HandshakeProtocolError reason))
        Emit (ParsedPong _) _ ->
          pure (Right ())
        Emit (ParsedOk _) rest ->
          continueWith rest
        Emit (ParsedPing _) rest -> do
          pongResult <- writeDataLazy (writer connectionApi) conn (transform Pong)
          case pongResult of
            Left err -> pure (Left (HandshakeTransportError err))
            Right () -> continueWith rest
        Emit (ParsedInfo info) rest -> do
          setServerInfo state info
          updateLogContextFromInfo state info
          continueWith rest
        Emit (ParsedErr err) _
          | isAuthenticationError err ->
              pure (Left (HandshakeAuthError (AuthError (show err))))
          | otherwise ->
              pure (Left (HandshakeProtocolError (show err)))
        Emit message _ ->
          pure (Left (HandshakeProtocolError ("unexpected frame before PONG: " ++ show message)))

    continueWith rest
      | BS.null rest = awaitPong mempty
      | otherwise = consumePongFrames rest

    isAuthenticationError (Err.ErrAuthViolation _)      = True
    isAuthenticationError (Err.ErrAuthTimeout _)        = True
    isAuthenticationError (Err.ErrAuthExpired _)        = True
    isAuthenticationError (Err.ErrAuthRevoked _)        = True
    isAuthenticationError (Err.ErrAccountAuthExpired _) = True
    isAuthenticationError _                             = False
