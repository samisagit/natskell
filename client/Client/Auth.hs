{-# LANGUAGE OverloadedStrings #-}

module Client.Auth
  ( buildConnectPayload
  , defaultConnect
  , logAuthMethod
  , logTlsConfig
  ) where

import qualified Auth.NKey       as NKey
import qualified Data.ByteString as BS
import           Lib.Logger      (AppM, LogLevel (..), logMessage)
import           Options
    ( Auth (..)
    , Config (..)
    , JWTTokenData
    , NKeyData
    , TLSCertData
    )
import qualified Types.Connect   as Connect (Connect (..))

defaultConnect :: Connect.Connect
defaultConnect = Connect.Connect
  { Connect.verbose = False
  , Connect.pedantic = True
  , Connect.tls_required = False
  , Connect.auth_token = Nothing
  , Connect.user = Nothing
  , Connect.pass = Nothing
  , Connect.name = Nothing
  , Connect.lang = "haskell"
  , Connect.version = "0.1.0"
  , Connect.protocol = Nothing
  , Connect.echo = Just True
  , Connect.sig = Nothing
  , Connect.jwt = Nothing
  , Connect.nkey = Nothing
  , Connect.no_responders = Just True
  , Connect.headers = Just True
  }

logAuthMethod :: Auth -> AppM ()
logAuthMethod auth = case auth of
  None               -> logMessage Info "no authentication method provided"
  AuthToken _        -> logMessage Info "using auth token"
  UserPass (user, _) -> logMessage Info $ "using user/pass: " ++ show user
  NKey _             -> logMessage Info "using nkey"
  JWT _              -> logMessage Info "using jwt"

logTlsConfig :: Maybe TLSCertData -> AppM ()
logTlsConfig tlsConfig = case tlsConfig of
  Nothing -> pure ()
  Just _  -> logMessage Info "using tls certificate"

buildConnectPayload :: Config -> Maybe BS.ByteString -> Bool -> Connect.Connect
buildConnectPayload cfg nonce tlsRequired =
  applyAuthToConnect base (auth cfg) nonce
  where
    base = (connectConfig cfg) { Connect.tls_required = tlsRequired }

applyAuthToConnect :: Connect.Connect -> Auth -> Maybe BS.ByteString -> Connect.Connect
applyAuthToConnect base authType nonce =
  case authType of
    None ->
      base
    AuthToken token ->
      (clearAuthFields base) { Connect.auth_token = nonEmpty token }
    UserPass (user, pass) ->
      let (user', pass') = userPassFields user pass
      in (clearAuthFields base) { Connect.user = user', Connect.pass = pass' }
    NKey seed ->
      applyNKeyAuth base seed nonce
    JWT jwtInput ->
      applyJwtAuth base jwtInput nonce

clearAuthFields :: Connect.Connect -> Connect.Connect
clearAuthFields base =
  base
    { Connect.auth_token = Nothing
    , Connect.user = Nothing
    , Connect.pass = Nothing
    , Connect.sig = Nothing
    , Connect.jwt = Nothing
    , Connect.nkey = Nothing
    }

nonEmpty :: BS.ByteString -> Maybe BS.ByteString
nonEmpty value =
  if BS.null value
    then Nothing
    else Just value

userPassFields :: BS.ByteString -> BS.ByteString -> (Maybe BS.ByteString, Maybe BS.ByteString)
userPassFields user pass
  | BS.null user || BS.null pass = (Nothing, Nothing)
  | otherwise = (Just user, Just pass)

applyNKeyAuth :: Connect.Connect -> NKeyData -> Maybe BS.ByteString -> Connect.Connect
applyNKeyAuth base seed nonce =
  case nonce of
    Nothing -> clearAuthFields base
    Just nonceValue ->
      case NKey.signNonceWithSeed seed nonceValue of
        Left _ -> clearAuthFields base
        Right (publicKey, signature) ->
          (clearAuthFields base)
            { Connect.nkey = nonEmpty publicKey
            , Connect.sig = nonEmpty signature
            }

applyJwtAuth :: Connect.Connect -> JWTTokenData -> Maybe BS.ByteString -> Connect.Connect
applyJwtAuth base jwtInput nonce =
  case NKey.parseJwtBundle jwtInput of
    Nothing ->
      clearAuthFields base
    Just bundle ->
      let base' = (clearAuthFields base) { Connect.jwt = nonEmpty (NKey.jwtToken bundle) }
      in case nonce of
          Nothing ->
            clearAuthFields base
          Just nonceValue ->
            case NKey.signNonceWithSeed (NKey.jwtSeed bundle) nonceValue of
              Left _ -> clearAuthFields base
              Right (_, signature) ->
                base' { Connect.sig = nonEmpty signature }
