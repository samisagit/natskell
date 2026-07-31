module Types.TLS
  ( TLSPublicKey
  , TLSPrivateKey
  , TLSCertData
  , TLSConfig (..)
  , TLSConfigSource
  , TLSConfigSourceFailure (..)
  , defaultTLSConfig
  , readTLSConfigSource
  ) where

import           Control.Exception
    ( SomeAsyncException
    , SomeException
    , fromException
    , throwIO
    , try
    )
import qualified Data.ByteString   as BS
import           Data.Maybe        (isJust)

type TLSPublicKey = BS.ByteString
type TLSPrivateKey = BS.ByteString
type TLSCertData = (TLSPublicKey, TLSPrivateKey)

-- | TLS trust and client-identity configuration.
data TLSConfig = TLSConfig
                   { tlsClientCertificate :: Maybe TLSCertData
                   , tlsRootCertificates  :: [BS.ByteString]
                   , tlsServerName        :: Maybe String
                   , tlsInsecure          :: Bool
                   }
  deriving (Eq)

-- | Reads a complete mutual-TLS configuration snapshot for one connection.
--
-- Sources are invoked immediately before each TLS handshake, including
-- reconnects and client resets.
type TLSConfigSource = IO (Either TLSConfigSourceFailure TLSConfig)

-- | Stable failure categories for renewable TLS configuration.
--
-- Raw source exceptions are intentionally not exposed because they can contain
-- certificate paths, secret-manager responses, or other private details.
data TLSConfigSourceFailure = TLSConfigSourceUnavailable | TLSConfigSourceIncomplete
  deriving (Eq, Show)

instance Show TLSConfig where
  show config =
    "TLSConfig {tlsClientCertificate = "
      ++ configured (isJust (tlsClientCertificate config))
      ++ ", tlsRootCertificates = "
      ++ show (length (tlsRootCertificates config))
      ++ " configured, tlsServerName = "
      ++ show (tlsServerName config)
      ++ ", tlsInsecure = "
      ++ show (tlsInsecure config)
      ++ "}"
    where
      configured True  = "<configured>"
      configured False = "Nothing"

defaultTLSConfig :: TLSConfig
defaultTLSConfig =
  TLSConfig
    { tlsClientCertificate = Nothing
    , tlsRootCertificates = []
    , tlsServerName = Nothing
    , tlsInsecure = False
    }

-- | Read and validate a source snapshot without leaking source exceptions.
--
-- Mutual-TLS sources must include their complete trust and identity material.
-- This prevents a certificate/key rotation from accidentally combining files
-- from different SVID generations with static client options.
readTLSConfigSource :: TLSConfigSource -> IO (Either TLSConfigSourceFailure TLSConfig)
readTLSConfigSource source = do
  result <- try source :: IO (Either SomeException (Either TLSConfigSourceFailure TLSConfig))
  case result of
    Left err ->
      case fromException err :: Maybe SomeAsyncException of
        Just _  -> throwIO err
        Nothing -> pure (Left TLSConfigSourceUnavailable)
    Right (Left err) -> pure (Left err)
    Right (Right config)
      | complete config -> pure (Right config)
      | otherwise       -> pure (Left TLSConfigSourceIncomplete)
  where
    complete config =
      isJust (tlsClientCertificate config)
        && not (null (tlsRootCertificates config))
        && isJust (tlsServerName config)
        && not (tlsInsecure config)
