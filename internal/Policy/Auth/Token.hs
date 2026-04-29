module Auth.Token
  ( auth
  ) where

import           Auth.Types

auth :: AuthTokenData -> Auth
auth = AuthToken
