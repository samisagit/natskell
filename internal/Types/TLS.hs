module Types.TLS
  ( TLSPublicKey
  , TLSPrivateKey
  , TLSCertData
  , TLSConfig (..)
  , defaultTLSConfig
  ) where

import qualified Data.ByteString as BS
import           Data.Maybe      (isJust)

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
