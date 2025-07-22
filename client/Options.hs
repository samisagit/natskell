module Options where
import qualified Data.ByteString as BS
import           Lib.CallOption
import           Lib.Logger
import           MSGView
import           Types
import qualified Types.Connect   as Connect

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

data Auth = None
          | UserPass UserPassData
          | NKey NKeyData
          | AuthToken AuthTokenData
          | JWT JWTTokenData
          | TLSCert TLSCertData
  deriving (Eq, Show)

data Config = Config
                { connectionAttempts :: Int
                , connectConfig      :: Connect.Connect
                , loggerConfig       :: LoggerConfig
                , auth               :: Auth
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
withTLSCert (pubKey, privKey) config = config { auth = TLSCert (pubKey, privKey) }

withLoggerConfig :: LoggerConfig -> ConfigOpts
withLoggerConfig loggerConfig config = config { loggerConfig = loggerConfig }

withConnectionAttempts :: Int -> ConfigOpts
withConnectionAttempts attempts config = config { connectionAttempts = attempts }

type PubOptions = (Maybe Payload, Maybe (MsgView -> IO ()), Maybe Headers)

-- | pubWithPayload is used to set the payload for a publish operation.
pubWithPayload :: Payload -> PubOptions -> PubOptions
pubWithPayload payload (_, callback, headers) = (Just payload, callback, headers)

-- | pubWithReplyCallback is used to set a callback for a reply to a publish operation.
pubWithReplyCallback :: (MsgView -> IO ()) -> PubOptions -> PubOptions
pubWithReplyCallback callback (payload, _, headers) = (payload, Just callback, headers)

-- | pubWithHeaders is used to set headers for a publish operation.
pubWithHeaders :: Headers -> PubOptions -> PubOptions
pubWithHeaders headers (payload, callback, _) = (payload, callback, Just headers)

