module Auth.NKey
  ( auth
  , authHandler
  , signNonceWithSeed
  ) where

import qualified Auth.NKey.Codec as Codec
import           Auth.Types

auth :: NKeyData -> Auth
auth seed = emptyAuth { authChallenge = Just (AuthNKeySeed seed) }

authHandler :: NKeyPublicKey -> SignatureHandler -> Auth
authHandler publicKey handler =
  emptyAuth { authChallenge = Just (AuthNKeyHandler publicKey handler) }

signNonceWithSeed :: NKeyData -> Nonce -> Either String (NKeyPublicKey, Signature)
signNonceWithSeed seed nonce = do
  (publicKey, signature) <- Codec.signNonceWithSeedRaw seed nonce
  pure (publicKey, Codec.encodeSignature signature)
