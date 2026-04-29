module Auth.NKey
  ( auth
  , signNonceWithSeed
  ) where

import           Auth.Types

auth :: NKeyData -> Auth
auth = AuthNKey
