module Auth.UserPass
  ( auth
  , authHandler
  ) where

import           Auth.Types

auth :: UserPassData -> Auth
auth = authHandler . pure . Right

authHandler :: UserPassHandler -> Auth
authHandler handler = emptyAuth { authUserPassHandler = Just handler }
