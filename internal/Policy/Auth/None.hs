module Auth.None
  ( auth
  ) where

import           Auth.Types

auth :: Auth
auth = AuthNone
