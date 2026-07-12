module NatsServerConfig
  ( NatsLogVerbosity (..)
  , NatsAuthUser (..)
  , NatsSubjectPermission (..)
  , NatsUserPermissions (..)
  , NatsJwtConfig (..)
  , NatsTlsConfig (..)
  , NatsConfigOption (..)
  , NatsConfigOptions
  , writeNatsServerConfigFile
  ) where

import           Data.List        (foldl', intercalate)
import           System.Directory (getTemporaryDirectory)
import           System.IO        (hClose, hPutStr, openTempFile)

data NatsLogVerbosity = NatsLogInfo | NatsLogDebug
  deriving (Eq)

data NatsAuthorization = NatsAuthorizationNone
                       | NatsAuthorizationToken String
                       | NatsAuthorizationUserPass String String
                       | NatsAuthorizationUsers [NatsAuthUser]
                       | NatsAuthorizationNKey String

data NatsAuthUser = NatsAuthUserPass String String
                  | NatsAuthUserPassWithPermissions String String NatsUserPermissions
                  | NatsAuthNKeyUser String
                  | NatsAuthUserName String

data NatsSubjectPermission = NatsSubjectPermission
                               { natsPermissionAllow :: [String]
                               , natsPermissionDeny  :: [String]
                               }

data NatsUserPermissions = NatsUserPermissions
                             { natsUserPublishPermissions :: Maybe NatsSubjectPermission
                             , natsUserSubscribePermissions :: Maybe NatsSubjectPermission
                             }

data NatsJwtConfig = NatsJwtConfig
                       { natsOperatorJwt        :: String
                       , natsJwtResolverPreload :: [(String, String)]
                       }

data NatsServerConfig = NatsServerConfig
                          { natsLogVerbosity    :: NatsLogVerbosity
                          , natsAuthorization   :: NatsAuthorization
                          , natsJwtConfig       :: Maybe NatsJwtConfig
                          , natsTlsConfig       :: Maybe NatsTlsConfig
                          , natsJetStream       :: Bool
                          , natsJetStreamDomain :: Maybe String
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
                      | WithJwtConfig NatsJwtConfig
                      | WithTlsConfig NatsTlsConfig
                      | WithJetStream
                      | WithJetStreamDomain String

type NatsConfigOptions = [NatsConfigOption]

defaultNatsServerConfig :: NatsServerConfig
defaultNatsServerConfig =
  NatsServerConfig
    { natsLogVerbosity = NatsLogInfo
    , natsAuthorization = NatsAuthorizationNone
    , natsJwtConfig = Nothing
    , natsTlsConfig = Nothing
    , natsJetStream = False
    , natsJetStreamDomain = Nothing
    }

writeNatsServerConfigFile :: NatsConfigOptions -> IO FilePath
writeNatsServerConfigFile options = do
  tmpDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tmpDir "nats-server.conf"
  hPutStr handle (renderNatsServerConfig (foldl' applyConfigOption defaultNatsServerConfig options))
  hClose handle
  pure path

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
    WithJwtConfig jwtConfig ->
      config { natsJwtConfig = Just jwtConfig }
    WithTlsConfig tlsConfig ->
      config { natsTlsConfig = Just tlsConfig }
    WithJetStream ->
      config { natsJetStream = True }
    WithJetStreamDomain domain ->
      config { natsJetStream = True, natsJetStreamDomain = Just domain }

renderNatsServerConfig :: NatsServerConfig -> String
renderNatsServerConfig config =
  unlines $
    logLines ++ renderAuthorization (natsAuthorization config)
      ++ maybe [] renderJwtConfig (natsJwtConfig config)
      ++ maybe [] renderTlsConfig (natsTlsConfig config)
      ++ renderJetStreamConfig (natsJetStream config) (natsJetStreamDomain config)
  where
    logLines =
      [ "debug: " ++ renderBool (natsLogVerbosity config == NatsLogDebug)
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
    NatsAuthUserPassWithPermissions name password permissions ->
      renderObject $
        [ "user: " ++ renderQuoted name
        , "password: " ++ renderQuoted password
        ]
          ++ renderUserPermissions permissions
    NatsAuthNKeyUser nkey ->
      renderObject
        [ "nkey: " ++ renderQuoted nkey
        ]
    NatsAuthUserName name ->
      renderObject
        [ "user: " ++ renderQuoted name
        ]

renderUserPermissions :: NatsUserPermissions -> [String]
renderUserPermissions permissions =
  ["permissions: {"]
    ++ indentLines 2
      ( renderSubjectPermission "publish" (natsUserPublishPermissions permissions)
          ++ renderSubjectPermission "subscribe" (natsUserSubscribePermissions permissions)
      )
    ++ ["}"]

renderSubjectPermission :: String -> Maybe NatsSubjectPermission -> [String]
renderSubjectPermission _ Nothing =
  []
renderSubjectPermission name (Just permission) =
  [name ++ ": {"]
    ++ indentLines 2
      ( renderPermissionSubjects "allow" (natsPermissionAllow permission)
          ++ renderPermissionSubjects "deny" (natsPermissionDeny permission)
      )
    ++ ["}"]

renderPermissionSubjects :: String -> [String] -> [String]
renderPermissionSubjects _ [] =
  []
renderPermissionSubjects name subjects =
  [name ++ ": " ++ renderQuotedList subjects]

renderJwtConfig :: NatsJwtConfig -> [String]
renderJwtConfig config =
  [ "operator: " ++ renderQuoted (natsOperatorJwt config)
  , "resolver: MEMORY"
  ]
    ++ renderBlock "resolver_preload" (renderResolverPreload (natsJwtResolverPreload config))

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

renderJetStreamConfig :: Bool -> Maybe String -> [String]
renderJetStreamConfig enabled domain =
  case (enabled, domain) of
    (False, _) ->
      []
    (True, Nothing) ->
      ["jetstream: enabled"]
    (True, Just domainName) ->
      renderBlock "jetstream"
        [ "domain: " ++ renderQuoted domainName
        ]

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

renderQuotedList :: [String] -> String
renderQuotedList values =
  "[" ++ intercalate ", " (map renderQuoted values) ++ "]"

escapeChar :: Char -> String
escapeChar char =
  case char of
    '\\' -> "\\\\"
    '"'  -> "\\\""
    _    -> [char]

renderBool :: Bool -> String
renderBool value =
  if value then "true" else "false"
