module Auth.Jwt
  ( JwtBundle (..)
  , auth
  , parseJwtBundle
  ) where

import           Auth.Types

auth :: JWTTokenData -> Auth
auth = AuthJWT
