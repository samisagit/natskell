module Auth.Types
  ( User
  , Pass
  , UserPassData
  , NKeyData
  , NKeyPublicKey
  , AuthTokenData
  , JWTTokenData
  , Nonce
  , Signature
  , AuthTokenHandler
  , UserPassHandler
  , JWTHandler
  , SignatureHandler
  , AuthError (..)
  , AuthContext (..)
  , AuthPatch (..)
  , Auth (..)
  , ChallengeAuth (..)
  , emptyAuth
  , emptyAuthPatch
  ) where

import qualified Data.ByteString as BS

type User = BS.ByteString
type Pass = BS.ByteString
type UserPassData = (User, Pass)

type NKeyData = BS.ByteString
type NKeyPublicKey = BS.ByteString

type AuthTokenData = BS.ByteString

type JWTTokenData = BS.ByteString

type Nonce = BS.ByteString
type Signature = BS.ByteString

type AuthTokenHandler = IO (Either AuthError AuthTokenData)
type UserPassHandler = IO (Either AuthError UserPassData)
type JWTHandler = IO (Either AuthError JWTTokenData)
type SignatureHandler = Nonce -> IO (Either AuthError Signature)

newtype AuthError = AuthError String
  deriving (Eq, Show)

newtype AuthContext = AuthContext { authNonce :: Maybe BS.ByteString }
  deriving (Eq, Show)

data AuthPatch = AuthPatch
                   { patchAuthToken :: Maybe BS.ByteString
                   , patchUser      :: Maybe BS.ByteString
                   , patchPass      :: Maybe BS.ByteString
                   , patchJwt       :: Maybe BS.ByteString
                   , patchNKey      :: Maybe BS.ByteString
                   , patchSig       :: Maybe BS.ByteString
                   }
  deriving (Eq)

instance Show AuthPatch where
  show patch =
    "AuthPatch {patchAuthToken = "
      ++ present (patchAuthToken patch)
      ++ ", patchUser = "
      ++ present (patchUser patch)
      ++ ", patchPass = "
      ++ present (patchPass patch)
      ++ ", patchJwt = "
      ++ present (patchJwt patch)
      ++ ", patchNKey = "
      ++ present (patchNKey patch)
      ++ ", patchSig = "
      ++ present (patchSig patch)
      ++ "}"
    where
      present Nothing  = "Nothing"
      present (Just _) = "<redacted>"

data ChallengeAuth = AuthNKeySeed NKeyData
                   | AuthNKeyHandler NKeyPublicKey SignatureHandler
                   | AuthJWTBundle JWTTokenData
                   | AuthJWTHandlers JWTHandler SignatureHandler

data Auth = Auth
              { authTokenHandler    :: Maybe AuthTokenHandler
              , authUserPassHandler :: Maybe UserPassHandler
              , authChallenge       :: Maybe ChallengeAuth
              }

emptyAuth :: Auth
emptyAuth = Auth Nothing Nothing Nothing

emptyAuthPatch :: AuthPatch
emptyAuthPatch =
  AuthPatch
    { patchAuthToken = Nothing
    , patchUser = Nothing
    , patchPass = Nothing
    , patchJwt = Nothing
    , patchNKey = Nothing
    , patchSig = Nothing
    }
