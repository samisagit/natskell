module Auth.Config
  ( authMethods
  , mergeAuth
  ) where

import           Auth.Types

-- | Merge auth configuration while preserving independent token and user/pass
-- methods. Challenge auth (NKey or JWT) remains mutually exclusive and the
-- right-hand configuration wins.
mergeAuth :: Auth -> Auth -> Auth
mergeAuth left right =
  Auth
    { authTokenHandler = preferRight (authTokenHandler left) (authTokenHandler right)
    , authUserPassHandler = preferRight (authUserPassHandler left) (authUserPassHandler right)
    , authChallenge = preferRight (authChallenge left) (authChallenge right)
    }

preferRight :: Maybe a -> Maybe a -> Maybe a
preferRight left Nothing     = left
preferRight _ right@(Just _) = right

authMethods :: Auth -> [String]
authMethods auth =
  tokenMethod ++ userPassMethod ++ challengeMethod
  where
    tokenMethod = ["auth token" | Just _ <- [authTokenHandler auth]]
    userPassMethod = ["user/pass" | Just _ <- [authUserPassHandler auth]]
    challengeMethod =
      case authChallenge auth of
        Nothing                    -> []
        Just (AuthNKeySeed _)      -> ["nkey"]
        Just (AuthNKeyHandler _ _) -> ["nkey"]
        Just (AuthJWTBundle _)     -> ["jwt"]
        Just (AuthJWTHandlers _ _) -> ["jwt"]
