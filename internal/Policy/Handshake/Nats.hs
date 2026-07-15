module Handshake.Nats
  ( HandshakeError (..)
  , performHandshake
  ) where

import           Auth.Resolver             (applyAuthPatch, buildAuthPatch)
import           Auth.Types
import qualified Data.ByteString           as BS
import           Data.List                 (isPrefixOf)
import           Data.Maybe                (fromMaybe, isJust)
import           Network.ConnectionAPI
    ( Conn
    , ConnectionAPI
    , TransportOption (..)
    , bufferRead
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
    , notifyServerError
    , setServerInfo
    , updateLogContextFromInfo
    )
import           State.Types
    ( ClientConfig (..)
    , serverErrorFromProtocol
    )
import           Transformers.Transformers (Transformer (transform))
import qualified Types.Connect             as Connect
import qualified Types.Err                 as Err
import qualified Types.Info                as I
import           Types.Ping                (Ping (Ping))
import           Types.Pong                (Pong (Pong))
import           Types.TLS                 (defaultTLSConfig)

data HandshakeError = HandshakeTransportError String
                    | HandshakeTLSError String
                    | HandshakeProtocolError String
                    | HandshakeAuthError AuthError
  deriving (Eq, Show)

handshakeControlFrameLimit :: Int
handshakeControlFrameLimit = 64 * 1024

performHandshake :: ConnectionAPI -> ParserAPI ParsedMessage -> ClientState -> Auth -> Conn -> String -> IO (Either HandshakeError ())
performHandshake connectionApi parserApi state auth conn host = handshake
  where
    handshake = do
      infoResult <- readInitialInfo
      case infoResult of
        Left err ->
          pure (Left err)
        Right (info, rest) -> do
          let cfg = config state
              tlsRequested = isJust (tlsConfig cfg) || Connect.tls_required (connectConfig cfg)
              tlsRequired = fromMaybe False (I.tls_required info)
              tlsAvailable = fromMaybe False (I.tls_available info)
              useTls = tlsRequired || (tlsRequested && tlsAvailable)
              transportOption =
                TransportOption
                  { transportHost = host
                  , transportTlsRequired = tlsRequired
                  , transportTlsRequested = tlsRequested && tlsAvailable
                  , transportTlsConfig =
                      if useTls
                        then Just (fromMaybe defaultTLSConfig (tlsConfig cfg))
                        else Nothing
                  , transportInitialBytes = rest
                  }
          if tlsRequested && not (tlsRequired || tlsAvailable)
            then pure (Left (HandshakeTLSError "server does not offer TLS"))
            else do
              transportResult <- configure connectionApi conn transportOption
              case transportResult of
                Left err ->
                  pure (Left (HandshakeTLSError err))
                Right () -> do
                  setServerInfo state info
                  updateLogContextFromInfo state info
                  let authContext = AuthContext { authNonce = I.nonce info }
                      connectPayload =
                        (connectConfig cfg)
                          { Connect.tls_required = useTls
                          }
                  authResult <- buildAuthPatch auth authContext
                  case authResult of
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
    readInitialInfo = readMore mempty
      where
        readMore acc = do
          result <- readData (reader connectionApi) conn 4096
          case result of
            Left err ->
              pure (Left (HandshakeTransportError err))
            Right chunk ->
              if BS.null chunk
                then pure (Left (HandshakeTransportError "read returned empty chunk before INFO"))
                else consumeInfoFrames (acc <> chunk)

        consumeInfoFrames bytes =
          case parse parserApi bytes of
            NeedMore ->
              retainIncompleteFrame "INFO" bytes readMore
            DropPrefix n _
              | n <= 0 ->
                  pure (Left (HandshakeProtocolError "malformed protocol frame"))
              | otherwise ->
                  continueInfoWith (BS.drop n bytes)
            Reject reason ->
              pure (Left (HandshakeProtocolError (reasonCategory reason)))
            Emit (ParsedInfo info) rest ->
              pure (Right (info, rest))
            Emit (ParsedErr err) _ -> do
              notifyServerError state (serverErrorFromProtocol err)
              if isAuthenticationError err
                then pure (Left (HandshakeAuthError (AuthError (show err))))
                else pure (Left (HandshakeProtocolError "server error before INFO"))
            Emit _ rest ->
              continueInfoWith rest

        continueInfoWith rest
          | BS.null rest = readMore mempty
          | otherwise = consumeInfoFrames rest

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
          retainIncompleteFrame "PONG" bytes awaitPong
        DropPrefix _ reason ->
          pure (Left (HandshakeProtocolError (reasonCategory reason)))
        Reject reason ->
          pure (Left (HandshakeProtocolError (reasonCategory reason)))
        Emit (ParsedPong _) rest -> do
          bufferRead (reader connectionApi) conn rest
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
        Emit (ParsedErr err) _ -> do
          notifyServerError state (serverErrorFromProtocol err)
          if isAuthenticationError err
            then pure (Left (HandshakeAuthError (AuthError (show err))))
            else pure (Left (HandshakeProtocolError "server error before PONG"))
        Emit message _ ->
          pure (Left (HandshakeProtocolError ("unexpected " ++ frameKind message ++ " frame before PONG")))

    continueWith rest
      | BS.null rest = awaitPong mempty
      | otherwise = consumePongFrames rest

    retainIncompleteFrame frame bytes continue
      | BS.length bytes > handshakeControlFrameLimit =
          pure . Left . HandshakeProtocolError $
            frame ++ " control frame exceeds 65536 byte limit"
      | otherwise = continue bytes

    isAuthenticationError (Err.ErrAuthViolation _)      = True
    isAuthenticationError (Err.ErrAuthTimeout _)        = True
    isAuthenticationError (Err.ErrAuthExpired _)        = True
    isAuthenticationError (Err.ErrAuthRevoked _)        = True
    isAuthenticationError (Err.ErrAccountAuthExpired _) = True
    isAuthenticationError _                             = False

    reasonCategory reason
      | "unknown protocol prefix" `isPrefixOf` reason = "unknown protocol prefix"
      | "invalid INFO" `isPrefixOf` reason = "invalid INFO frame"
      | "invalid MSG" `isPrefixOf` reason = "invalid MSG frame"
      | "invalid HMSG" `isPrefixOf` reason = "invalid HMSG frame"
      | otherwise = "malformed protocol frame"

    frameKind (ParsedMsg _)               = "MSG"
    frameKind (ParsedInfo _)              = "INFO"
    frameKind (ParsedPing _)              = "PING"
    frameKind (ParsedPong _)              = "PONG"
    frameKind (ParsedOk _)                = "+OK"
    frameKind (ParsedErr _)               = "-ERR"
    frameKind (ParsedMessageTooLarge _ _) = "oversized message"
