module Auth.NKey
  ( auth
  , authHandler
  , signNonceWithSeed
  ) where

import           Auth.Types

auth :: NKeyData -> Auth
auth seed = emptyAuth { authChallenge = Just (AuthNKeySeed seed) }

authHandler :: NKeyPublicKey -> SignatureHandler -> Auth
authHandler publicKey handler =
  emptyAuth { authChallenge = Just (AuthNKeyHandler publicKey handler) }
