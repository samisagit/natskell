module Options where
import qualified Data.ByteString as BS
import           Data.Time.Clock (NominalDiffTime)
import           Lib.CallOption
import           Lib.Logger
import           MSGView
import           Types
import qualified Types.Connect   as Connect
import qualified Types.Err       as Err

type ConfigOpts = CallOption Config

type User = BS.ByteString
type Pass = BS.ByteString
type UserPassData = (User, Pass)

type NKeyData = BS.ByteString

type AuthTokenData = BS.ByteString

type JWTTokenData = BS.ByteString

type TLSPublicKey = BS.ByteString
type TLSPrivateKey = BS.ByteString
type TLSCertData = (TLSPublicKey, TLSPrivateKey)

data ClientExitReason = ExitClosedByUser
                      | ExitRetriesExhausted (Maybe String)
                      | ExitServerError Err.Err
                      | ExitResetRequested
  deriving (Eq, Show)

data Auth = None
          | UserPass UserPassData
          | NKey NKeyData
          | AuthToken AuthTokenData
          | JWT JWTTokenData
  deriving (Eq, Show)

data Config = Config
                { connectionAttempts :: Int
                , connectConfig      :: Connect.Connect
                , loggerConfig       :: LoggerConfig
                , auth               :: Auth
                , tlsCert            :: Maybe TLSCertData
                , exitAction         :: ClientExitReason -> IO ()
                , connectOptions     :: [(String, Int)]
                }

-- | withConnectName sets the client name sent in CONNECT.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- client <- newClient [(\"127.0.0.1\", 4222)] [withConnectName \"example-client\"]
-- @
withConnectName :: BS.ByteString -> ConfigOpts
withConnectName name config = config { connectConfig = (connectConfig config) { Connect.name = Just name } }

-- | withAuthToken configures token authentication.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- client <- newClient [(\"127.0.0.1\", 4222)] [withAuthToken \"s3cr3t\"]
-- @
withAuthToken :: AuthTokenData -> ConfigOpts
withAuthToken token config = config { auth = AuthToken token }

-- | withUserPass configures user/password authentication.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- client <- newClient [(\"127.0.0.1\", 4222)] [withUserPass (\"alice\", \"secret\")]
-- @
withUserPass :: UserPassData -> ConfigOpts
withUserPass (user, pass) config = config { auth = UserPass (user, pass) }

-- | withNKey configures NKey authentication.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- client <- newClient [(\"127.0.0.1\", 4222)] [withNKey \"SU...\"]
-- @
withNKey :: NKeyData -> ConfigOpts
withNKey nkey config = config { auth = NKey nkey }

-- | withJWT configures JWT authentication.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import qualified Data.ByteString as BS
--
-- creds <- BS.readFile \"user.creds\"
-- client <- newClient [(\"127.0.0.1\", 4222)] [withJWT creds]
-- @
withJWT :: JWTTokenData -> ConfigOpts
withJWT jwt config = config { auth = JWT jwt }

-- | withTLSCert configures client TLS credentials.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import qualified Data.ByteString as BS
--
-- certPem <- BS.readFile \"client.pem\"
-- keyPem <- BS.readFile \"client.key\"
-- client <- newClient [(\"127.0.0.1\", 4222)] [withTLSCert (certPem, keyPem)]
-- @
withTLSCert :: TLSCertData -> ConfigOpts
withTLSCert (pubKey, privKey) config = config { tlsCert = Just (pubKey, privKey) }

-- | withLoggerConfig replaces the default logger configuration.
--
-- __Examples:__
--
-- @
-- import Control.Concurrent.STM (newTMVarIO)
--
-- lock <- newTMVarIO ()
-- let logger = LoggerConfig Debug print lock
-- client <- newClient [(\"127.0.0.1\", 4222)] [withLoggerConfig logger]
-- @
withLoggerConfig :: LoggerConfig -> ConfigOpts
withLoggerConfig loggerConfig config = config { loggerConfig = loggerConfig }

-- | withConnectionAttempts sets the number of connection retries.
--
-- __Examples:__
--
-- @
-- client <- newClient [(\"127.0.0.1\", 4222)] [withConnectionAttempts 10]
-- @
withConnectionAttempts :: Int -> ConfigOpts
withConnectionAttempts attempts config = config { connectionAttempts = attempts }

-- | withExitAction runs a callback when the client exits.
--
-- __Examples:__
--
-- @
-- client <- newClient [(\"127.0.0.1\", 4222)] [withExitAction print]
-- @
withExitAction :: (ClientExitReason -> IO ()) -> ConfigOpts
withExitAction action config = config { exitAction = action }

type PubOptions = (Maybe Payload, Maybe (Maybe MsgView -> IO ()), Maybe Headers)

newtype SubscribeConfig = SubscribeConfig { subscriptionExpiry :: NominalDiffTime }

defaultSubscribeConfig :: SubscribeConfig
defaultSubscribeConfig = SubscribeConfig 5.0

type SubscribeOpts = CallOption SubscribeConfig

-- | withSubscriptionExpiry sets the reply subscription expiry in seconds.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- subscribe client \"events.created\" [withSubscriptionExpiry 2] print
-- @
withSubscriptionExpiry :: NominalDiffTime -> SubscribeOpts
withSubscriptionExpiry expirySeconds cfg = cfg { subscriptionExpiry = expirySeconds }

-- | withPayload is used to set the payload for a publish operation.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- publish client \"updates\" [withPayload \"hello\"]
-- @
withPayload :: Payload -> PubOptions -> PubOptions
withPayload payload (_, callback, headers) = (Just payload, callback, headers)

-- | withReplyCallback is used to set a callback for a reply to a publish operation.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- publish client \"service.echo\" [withReplyCallback print]
-- @
withReplyCallback :: (Maybe MsgView -> IO ()) -> PubOptions -> PubOptions
withReplyCallback callback (payload, _, headers) = (payload, Just callback, headers)

-- | withHeaders is used to set headers for a publish operation.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- publish client \"updates\" [withHeaders [(\"source\", \"test\")]]
-- @
withHeaders :: Headers -> PubOptions -> PubOptions
withHeaders headers (payload, callback, _) = (payload, callback, Just headers)
