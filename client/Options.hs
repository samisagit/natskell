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

withConnectName :: BS.ByteString -> ConfigOpts
withConnectName name config = config { connectConfig = (connectConfig config) { Connect.name = Just name } }

withAuthToken :: AuthTokenData -> ConfigOpts
withAuthToken token config = config { auth = AuthToken token }

withUserPass :: UserPassData -> ConfigOpts
withUserPass (user, pass) config = config { auth = UserPass (user, pass) }

withNKey :: NKeyData -> ConfigOpts
withNKey nkey config = config { auth = NKey nkey }

withJWT :: JWTTokenData -> ConfigOpts
withJWT jwt config = config { auth = JWT jwt }

withTLSCert :: TLSCertData -> ConfigOpts
withTLSCert (pubKey, privKey) config = config { tlsCert = Just (pubKey, privKey) }

withLoggerConfig :: LoggerConfig -> ConfigOpts
withLoggerConfig loggerConfig config = config { loggerConfig = loggerConfig }

withConnectionAttempts :: Int -> ConfigOpts
withConnectionAttempts attempts config = config { connectionAttempts = attempts }

withExitAction :: (ClientExitReason -> IO ()) -> ConfigOpts
withExitAction action config = config { exitAction = action }

type PubOptions = (Maybe Payload, Maybe (Maybe MsgView -> IO ()), Maybe Headers)

newtype SubscribeConfig = SubscribeConfig { subscriptionExpiry :: NominalDiffTime }

defaultSubscribeConfig :: SubscribeConfig
defaultSubscribeConfig = SubscribeConfig 5.0

type SubscribeOpts = CallOption SubscribeConfig

withSubscriptionExpiry :: NominalDiffTime -> SubscribeOpts
withSubscriptionExpiry expirySeconds cfg = cfg { subscriptionExpiry = expirySeconds }

-- | withPayload is used to set the payload for a publish operation.
withPayload :: Payload -> PubOptions -> PubOptions
withPayload payload (_, callback, headers) = (Just payload, callback, headers)

-- | withReplyCallback is used to set a callback for a reply to a publish operation.
withReplyCallback :: (Maybe MsgView -> IO ()) -> PubOptions -> PubOptions
withReplyCallback callback (payload, _, headers) = (payload, Just callback, headers)

-- | withHeaders is used to set headers for a publish operation.
withHeaders :: Headers -> PubOptions -> PubOptions
withHeaders headers (payload, callback, _) = (payload, callback, Just headers)
