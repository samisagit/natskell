module Auth.Token
  ( auth
  , authHandler
  ) where

import           Auth.Types

auth :: AuthTokenData -> Auth
auth = authHandler . pure . Right

authHandler :: AuthTokenHandler -> Auth
authHandler handler = emptyAuth { authTokenHandler = Just handler }
