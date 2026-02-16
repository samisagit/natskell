{-# LANGUAGE OverloadedStrings #-}

module AuthConnectSpec (spec) where

import           Client
import           Control.Concurrent.STM (newTMVarIO)
import           Options
import           Test.Hspec
import           Types.Connect          (Connect (..))

spec :: Spec
spec = do
  logger <- runIO mkLoggerConfig
  let mkConfig authType base = Config
        { connectionAttempts = 1
        , connectConfig = base
        , loggerConfig = logger
        , auth = authType
        , exitAction = const (pure ())
        , connectOptions = []
        }
  describe "CONNECT auth payload" $ do
    it "sets user/pass from auth and clears other auth fields" $ do
      let payload = buildConnectPayload (mkConfig (UserPass ("user", "pass")) baseConnect)
      user payload `shouldBe` Just "user"
      pass payload `shouldBe` Just "pass"
      auth_token payload `shouldBe` Nothing
      sig payload `shouldBe` Nothing
      jwt payload `shouldBe` Nothing
      nkey payload `shouldBe` Nothing
      tls_required payload `shouldBe` True

    it "sets auth token from auth and clears other auth fields" $ do
      let payload = buildConnectPayload (mkConfig (AuthToken "new-token") baseConnect)
      auth_token payload `shouldBe` Just "new-token"
      user payload `shouldBe` Nothing
      pass payload `shouldBe` Nothing
      sig payload `shouldBe` Nothing
      jwt payload `shouldBe` Nothing
      nkey payload `shouldBe` Nothing
      tls_required payload `shouldBe` True

    it "does not set empty user/pass" $ do
      let payload = buildConnectPayload (mkConfig (UserPass ("", "")) emptyConnect)
      user payload `shouldBe` Nothing
      pass payload `shouldBe` Nothing
      tls_required payload `shouldBe` True

mkLoggerConfig :: IO LoggerConfig
mkLoggerConfig = do
  lock <- newTMVarIO ()
  pure $ LoggerConfig Debug (const (pure ())) lock

baseConnect :: Connect
baseConnect =
  Connect
    { verbose = False
    , pedantic = True
    , tls_required = True
    , auth_token = Just "token"
    , user = Just "old-user"
    , pass = Just "old-pass"
    , name = Just "name"
    , lang = "Haskell"
    , version = "1"
    , protocol = Nothing
    , echo = Just True
    , sig = Just "sig"
    , jwt = Just "jwt"
    , nkey = Just "nkey"
    , no_responders = Just True
    , headers = Just True
    }

emptyConnect :: Connect
emptyConnect =
  baseConnect
    { auth_token = Nothing
    , user = Nothing
    , pass = Nothing
    , sig = Nothing
    , jwt = Nothing
    , nkey = Nothing
    }
