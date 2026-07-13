module Types.TLS
  ( TLSPublicKey
  , TLSPrivateKey
  , TLSCertData
  , TLSConfig (..)
  , defaultTLSConfig
  ) where

import qualified Data.ByteString as BS

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
  deriving (Eq, Show)

defaultTLSConfig :: TLSConfig
defaultTLSConfig =
  TLSConfig
    { tlsClientCertificate = Nothing
    , tlsRootCertificates = []
    , tlsServerName = Nothing
    , tlsInsecure = False
    }
