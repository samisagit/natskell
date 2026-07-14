{-# LANGUAGE TypeApplications #-}

module Auth.Resolver
  ( applyAuthPatch
  , buildAuthPatch
  ) where

import           Auth.Credentials  (JwtBundle (..), parseJwtBundle)
import qualified Auth.NKey.Codec   as NKeyCodec
import           Auth.Types
import           Control.Exception
    ( SomeAsyncException
    , SomeException
    , displayException
    , fromException
    , throwIO
    , try
    )
import qualified Data.ByteString   as BS
import qualified Types.Connect     as Connect

buildAuthPatch :: Auth -> AuthContext -> IO (Either AuthError AuthPatch)
buildAuthPatch auth ctx = do
  tokenResult <- resolveOptional "auth token" validateToken (authTokenHandler auth)
  userPassResult <- resolveOptional "user/pass" validateUserPass (authUserPassHandler auth)
  challengeResult <- buildChallengePatch (authChallenge auth) ctx
  pure $ do
    token <- tokenResult
    userPass <- userPassResult
    challengePatch <- challengeResult
    pure challengePatch
      { patchAuthToken = token
      , patchUser = fst <$> userPass
      , patchPass = snd <$> userPass
      }

resolveOptional
  :: String
  -> (a -> Either AuthError ())
  -> Maybe (IO (Either AuthError a))
  -> IO (Either AuthError (Maybe a))
resolveOptional _ _ Nothing = pure (Right Nothing)
resolveOptional label validate (Just handler) = do
  result <- runAuthHandler label handler
  pure $ do
    value <- result
    validate value
    pure (Just value)

buildChallengePatch :: Maybe ChallengeAuth -> AuthContext -> IO (Either AuthError AuthPatch)
buildChallengePatch Nothing _ = pure (Right emptyAuthPatch)
buildChallengePatch (Just challenge) ctx =
  case requireNonce ctx of
    Left err -> pure (Left err)
    Right nonce ->
      case challenge of
        AuthNKeySeed seed ->
          pure $ do
            (publicKey, signature) <- signNonceWithSeedRaw seed nonce
            pure emptyAuthPatch
              { patchNKey = Just publicKey
              , patchSig = Just (NKeyCodec.encodeSignature signature)
              }
        AuthNKeyHandler publicKey signatureHandler ->
          case validatePublicKey publicKey of
            Left err -> pure (Left err)
            Right () -> do
              signatureResult <- resolveSignature signatureHandler nonce
              pure $ do
                signature <- signatureResult
                pure emptyAuthPatch
                  { patchNKey = Just publicKey
                  , patchSig = Just signature
                  }
        AuthJWTBundle creds ->
          pure $ do
            bundle <-
              maybe (Left (AuthError "jwt credentials bundle is invalid")) Right (parseJwtBundle creds)
            (_, signature) <- signNonceWithSeedRaw (jwtSeed bundle) nonce
            pure emptyAuthPatch
              { patchJwt = Just (jwtToken bundle)
              , patchSig = Just (NKeyCodec.encodeSignature signature)
              }
        AuthJWTHandlers jwtHandler signatureHandler -> do
          jwtResult <- runAuthHandler "jwt" jwtHandler
          case jwtResult >>= validateRawJwtValue of
            Left err -> pure (Left err)
            Right jwt -> do
              signatureResult <- resolveSignature signatureHandler nonce
              pure $ do
                signature <- signatureResult
                pure emptyAuthPatch
                  { patchJwt = Just jwt
                  , patchSig = Just signature
                  }

signNonceWithSeedRaw
  :: BS.ByteString
  -> BS.ByteString
  -> Either AuthError (BS.ByteString, BS.ByteString)
signNonceWithSeedRaw seed nonce =
  either (Left . AuthError) Right (NKeyCodec.signNonceWithSeedRaw seed nonce)

resolveSignature :: SignatureHandler -> Nonce -> IO (Either AuthError BS.ByteString)
resolveSignature handler nonce = do
  result <- runAuthHandler "nonce signature" (handler nonce)
  pure $ do
    signature <- result
    if BS.length signature == 64
      then Right (NKeyCodec.encodeSignature signature)
      else Left (AuthError "nonce signature must contain 64 raw bytes")

runAuthHandler :: String -> IO (Either AuthError a) -> IO (Either AuthError a)
runAuthHandler label action = do
  result <- try @SomeException action
  case result of
    Right value -> pure value
    Left err ->
      case fromException err :: Maybe SomeAsyncException of
        Just _  -> throwIO err
        Nothing -> pure (Left (AuthError (label ++ " handler failed: " ++ displayException err)))

applyAuthPatch :: AuthPatch -> Connect.Connect -> Connect.Connect
applyAuthPatch patch connect =
  connect
    { Connect.auth_token = patchAuthToken patch
    , Connect.user = patchUser patch
    , Connect.pass = patchPass patch
    , Connect.jwt = patchJwt patch
    , Connect.nkey = patchNKey patch
    , Connect.sig = patchSig patch
    }

validateToken :: AuthTokenData -> Either AuthError ()
validateToken token
  | BS.null token = Left (AuthError "auth token must not be empty")
  | otherwise = Right ()

validateUserPass :: UserPassData -> Either AuthError ()
validateUserPass (user, pass)
  | BS.null user = Left (AuthError "user must not be empty")
  | BS.null pass = Left (AuthError "pass must not be empty")
  | otherwise = Right ()

validatePublicKey :: NKeyPublicKey -> Either AuthError ()
validatePublicKey =
  either (Left . AuthError . ("nkey public key is invalid: " ++)) Right
    . NKeyCodec.validateUserPublicKey

validateRawJwtValue :: JWTTokenData -> Either AuthError JWTTokenData
validateRawJwtValue token
  | BS.null token = Left (AuthError "jwt must not be empty")
  | otherwise = Right token

requireNonce :: AuthContext -> Either AuthError BS.ByteString
requireNonce (AuthContext maybeNonce) =
  maybe (Left (AuthError "auth method requires a server nonce")) Right maybeNonce
