module Auth.UserPass
  ( auth
  ) where

import           Auth.Types

auth :: UserPassData -> Auth
auth = AuthUserPass
