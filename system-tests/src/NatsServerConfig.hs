module NatsServerConfig
  ( NatsLogVerbosity (..)
  , NatsAuthUser (..)
  , NatsAuthCallout (..)
  , NatsJwtConfig (..)
  , NatsJwtResolver (..)
  , NatsTlsConfig (..)
  , NatsConfigOption (..)
  , NatsConfigOptions
  , renderConfig
  , writeNatsServerConfigFile
  ) where

import           Data.List        (foldl')
import           System.Directory (getTemporaryDirectory)
import           System.IO        (hClose, hPutStr, openTempFile)

data NatsLogVerbosity = NatsLogInfo | NatsLogDebug | NatsLogTrace
  deriving (Eq)

data NatsAuthorization = NatsAuthorizationNone
                       | NatsAuthorizationToken String
                       | NatsAuthorizationUserPass String String
                       | NatsAuthorizationUsers [NatsAuthUser]
                       | NatsAuthorizationNKey String
                       | NatsAuthorizationAuthCallout NatsAuthCallout
                       | NatsAuthorizationCustom [String]

data NatsAuthUser = NatsAuthUserPass String String
                  | NatsAuthNKeyUser String
                  | NatsAuthUserName String

data NatsAuthCallout = NatsAuthCallout
                         { natsCalloutIssuer  :: String
                         , natsCalloutAccount :: String
                         , natsCalloutUsers   :: [NatsAuthUser]
                         }

data NatsJwtResolver = NatsJwtResolverFull FilePath
                     | NatsJwtResolverMemory [(String, String)]
                     | NatsJwtResolverCustom [String]

data NatsJwtConfig = NatsJwtConfig
                       { natsOperatorJwt   :: String
                       , natsSystemAccount :: Maybe String
                       , natsJwtResolver   :: NatsJwtResolver
                       }

data NatsServerConfig = NatsServerConfig
                          { natsLogVerbosity  :: NatsLogVerbosity
                          , natsAuthorization :: NatsAuthorization
                          , natsJwtConfig     :: Maybe NatsJwtConfig
                          , natsTlsConfig     :: Maybe NatsTlsConfig
                          }

data NatsTlsConfig = NatsTlsConfig
                       { natsTlsCertFile     :: FilePath
                       , natsTlsKeyFile      :: FilePath
                       , natsTlsCaFile       :: Maybe FilePath
                       , natsTlsVerify       :: Bool
                       , natsTlsVerifyAndMap :: Bool
                       , natsTlsTimeout      :: Maybe Int
                       }

data NatsConfigOption = WithLogVerbosity NatsLogVerbosity
                      | WithUserPass String String
                      | WithToken String
                      | WithUsers [NatsAuthUser]
                      | WithNKey String
                      | WithAuthCallout NatsAuthCallout
                      | WithAuthorizationCustom [String]
                      | WithJwtConfig NatsJwtConfig
                      | WithTlsConfig NatsTlsConfig

type NatsConfigOptions = [NatsConfigOption]

defaultNatsServerConfig :: NatsServerConfig
defaultNatsServerConfig =
  NatsServerConfig
    { natsLogVerbosity = NatsLogInfo
    , natsAuthorization = NatsAuthorizationNone
    , natsJwtConfig = Nothing
    , natsTlsConfig = Nothing
    }

writeNatsServerConfigFile :: NatsConfigOptions -> IO FilePath
writeNatsServerConfigFile options = do
  tmpDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tmpDir "nats-server.conf"
  hPutStr handle (renderConfig options)
  hClose handle
  pure path

renderConfig :: NatsConfigOptions -> String
renderConfig =
  renderNatsServerConfig . applyConfigOptions

applyConfigOptions :: NatsConfigOptions -> NatsServerConfig
applyConfigOptions =
  foldl' applyConfigOption defaultNatsServerConfig

applyConfigOption :: NatsServerConfig -> NatsConfigOption -> NatsServerConfig
applyConfigOption config option =
  case option of
    WithLogVerbosity verbosity ->
      config { natsLogVerbosity = verbosity }
    WithUserPass user password ->
      config { natsAuthorization = NatsAuthorizationUserPass user password }
    WithToken token ->
      config { natsAuthorization = NatsAuthorizationToken token }
    WithUsers users ->
      config { natsAuthorization = NatsAuthorizationUsers users }
    WithNKey nkey ->
      config { natsAuthorization = NatsAuthorizationNKey nkey }
    WithAuthCallout callout ->
      config { natsAuthorization = NatsAuthorizationAuthCallout callout }
    WithAuthorizationCustom lines ->
      config { natsAuthorization = NatsAuthorizationCustom lines }
    WithJwtConfig jwtConfig ->
      config { natsJwtConfig = Just jwtConfig }
    WithTlsConfig tlsConfig ->
      config { natsTlsConfig = Just tlsConfig }

renderNatsServerConfig :: NatsServerConfig -> String
renderNatsServerConfig config =
  unlines $
    logLines ++ renderAuthorization (natsAuthorization config)
      ++ maybe [] renderJwtConfig (natsJwtConfig config)
      ++ maybe [] renderTlsConfig (natsTlsConfig config)
  where
    logLines =
      [ "debug: " ++ renderBool (natsLogVerbosity config /= NatsLogInfo)
      , "trace: " ++ renderBool (natsLogVerbosity config == NatsLogTrace)
      , "logtime: true"
      ]

renderAuthorization :: NatsAuthorization -> [String]
renderAuthorization auth =
  case auth of
    NatsAuthorizationNone ->
      []
    NatsAuthorizationToken token ->
      renderBlock "authorization"
        [ "token: " ++ renderQuoted token
        ]
    NatsAuthorizationUserPass user password ->
      renderBlock "authorization"
        [ "user: " ++ renderQuoted user
        , "password: " ++ renderQuoted password
        ]
    NatsAuthorizationUsers users ->
      renderBlock "authorization" (renderUsers users)
    NatsAuthorizationNKey nkey ->
      renderBlock "authorization" (renderUsers [NatsAuthNKeyUser nkey])
    NatsAuthorizationAuthCallout callout ->
      renderBlock "authorization" (renderAuthCallout callout)
    NatsAuthorizationCustom lines ->
      renderBlock "authorization" lines

renderAuthCallout :: NatsAuthCallout -> [String]
renderAuthCallout callout =
  renderBlock "auth_callout"
    ( [ "issuer: " ++ renderQuoted (natsCalloutIssuer callout)
      , "account: " ++ renderQuoted (natsCalloutAccount callout)
      ]
        ++ renderUsers (natsCalloutUsers callout)
    )

renderUsers :: [NatsAuthUser] -> [String]
renderUsers users =
  ["users = ["]
    ++ indentLines 2 (concatMap renderUser users)
    ++ ["]"]

renderUser :: NatsAuthUser -> [String]
renderUser user =
  case user of
    NatsAuthUserPass name password ->
      renderObject
        [ "user: " ++ renderQuoted name
        , "password: " ++ renderQuoted password
        ]
    NatsAuthNKeyUser nkey ->
      renderObject
        [ "nkey: " ++ renderQuoted nkey
        ]
    NatsAuthUserName name ->
      renderObject
        [ "user: " ++ renderQuoted name
        ]

renderJwtConfig :: NatsJwtConfig -> [String]
renderJwtConfig config =
  [ "operator: " ++ renderQuoted (natsOperatorJwt config)
  ]
    ++ maybe [] (\account -> ["system_account: " ++ renderQuoted account]) (natsSystemAccount config)
    ++ renderJwtResolver (natsJwtResolver config)

renderJwtResolver :: NatsJwtResolver -> [String]
renderJwtResolver resolver =
  case resolver of
    NatsJwtResolverFull dir ->
      renderBlock "resolver"
        [ "type: full"
        , "dir: " ++ renderQuoted dir
        ]
    NatsJwtResolverMemory preloads ->
      "resolver: MEMORY"
        : renderBlock "resolver_preload" (renderResolverPreload preloads)
    NatsJwtResolverCustom lines ->
      renderBlock "resolver" lines

renderTlsConfig :: NatsTlsConfig -> [String]
renderTlsConfig config =
  renderBlock "tls"
    ( [ "cert_file: " ++ renderQuoted (natsTlsCertFile config)
      , "key_file: " ++ renderQuoted (natsTlsKeyFile config)
      , "verify: " ++ renderBool (natsTlsVerify config)
      , "verify_and_map: " ++ renderBool (natsTlsVerifyAndMap config)
      ]
        ++ maybe [] (\caFile -> ["ca_file: " ++ renderQuoted caFile]) (natsTlsCaFile config)
        ++ maybe [] (\timeout -> ["timeout: " ++ show timeout]) (natsTlsTimeout config)
    )

renderResolverPreload :: [(String, String)] -> [String]
renderResolverPreload =
  map (\(account, jwt) -> account ++ ": " ++ renderQuoted jwt)

renderObject :: [String] -> [String]
renderObject fields =
  ["{"] ++ indentLines 2 fields ++ ["}"]

renderBlock :: String -> [String] -> [String]
renderBlock name contents =
  [name ++ " {"] ++ indentLines 2 contents ++ ["}"]

indentLines :: Int -> [String] -> [String]
indentLines width =
  map (replicate width ' ' ++)

renderQuoted :: String -> String
renderQuoted value =
  "\"" ++ concatMap escapeChar value ++ "\""

escapeChar :: Char -> String
escapeChar char =
  case char of
    '\\' -> "\\\\"
    '"'  -> "\\\""
    _    -> [char]

renderBool :: Bool -> String
renderBool value =
  if value then "true" else "false"
