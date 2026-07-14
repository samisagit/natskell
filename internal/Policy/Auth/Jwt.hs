module Auth.Jwt
  ( JwtBundle (..)
  , auth
  , authHandlers
  , parseJwtBundle
  ) where

import           Auth.Credentials (JwtBundle (..), parseJwtBundle)
import           Auth.Types

auth :: JWTTokenData -> Auth
auth creds = emptyAuth { authChallenge = Just (AuthJWTBundle creds) }

authHandlers :: JWTHandler -> SignatureHandler -> Auth
authHandlers jwtHandler signatureHandler =
  emptyAuth { authChallenge = Just (AuthJWTHandlers jwtHandler signatureHandler) }
