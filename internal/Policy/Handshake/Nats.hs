module Handshake.Nats
  ( HandshakeError (..)
  , performHandshake
  ) where

import           Auth.Types
import           Control.Monad             (unless)
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
    ( ParserAPI
    , Suggestion (..)
    , parse
    , solveErr
    )
import           Parser.Nats               (ParsedMessage (..))
import           State.Store
    ( ClientState
    , config
    , setServerInfo
    , updateLogContextFromInfo
    )
import           State.Types               (ClientConfig (..))
import           Transformers.Transformers (Transformer (transform))
import qualified Types.Connect             as Connect
import qualified Types.Info                as I

data HandshakeError = HandshakeTransportError String
                    | HandshakeProtocolError String
                    | HandshakeAuthError AuthError
  deriving (Eq, Show)

performHandshake :: ConnectionAPI -> ParserAPI ParsedMessage -> ClientState -> Auth -> Conn -> String -> IO (Either HandshakeError ())
performHandshake connectionApi parserApi state auth conn host = do
  infoResult <- readInitialInfo
  case infoResult of
    Left err ->
      pure (Left (HandshakeProtocolError err))
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
                      (transform (applyAuthPatch patch connectPayload))
                  case writeResult of
                    Left err ->
                      pure (Left (HandshakeTransportError err))
                    Right () ->
                      pure (Right ())
  where
    readInitialInfo = go mempty
      where
        go acc = do
          result <- readData (reader connectionApi) conn 4096
          case result of
            Left err ->
              pure (Left err)
            Right chunk ->
              if BS.null chunk
                then pure (Left "read returned empty chunk before INFO")
                else do
                  let bytes = acc <> chunk
                  case parse parserApi bytes of
                    Left err ->
                      case solveErr parserApi err (BS.length bytes) of
                        SuggestPull ->
                          go bytes
                        SuggestDrop n _ ->
                          go (BS.drop n bytes)
                    Right (ParsedInfo info, rest) ->
                      pure (Right (info, rest))
                    Right (ParsedErr err, _) ->
                      pure (Left ("server error before INFO: " ++ show err))
                    Right (_, rest) -> do
                      unless (BS.null rest) (pure ())
                      go rest
