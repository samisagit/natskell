{-# LANGUAGE OverloadedStrings #-}

module ClientAuthJwtCredsSpec (spec) where

import           API                    (Client (..))
import           Client
import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM
import           NatsServerConfig
import           Test.Hspec
import           TestSupport

spec :: Spec
spec =
  systemTest . describe "client auth" $ do
      let loggerOptions = testLoggerOptions
      operatorJwt <- runIO (readFixtureText "jwt/operator.jwt")
      accountJwt <- runIO (readFixtureText "jwt/account.jwt")
      accountPub <- runIO (readFixtureText "jwt/account.pub")
      userCreds <- runIO (readFixtureBytesRaw "jwt/user.creds")
      let serverOptions =
            [ WithLogVerbosity NatsLogDebug
            , WithJwtConfig NatsJwtConfig
                { natsOperatorJwt = operatorJwt
                , natsJwtResolverPreload = [(accountPub, accountJwt)]
                }
            ]
      around (withNatsContainerConfigNamed "0f7d6f2c-4c59-4cc0-a72c-8b4b0ed469d5" serverOptions) $ do
        it "authenticates with jwt creds file" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOptions =
                [ withConnectName "auth-jwt-creds"
                , withExitAction (atomically . putTMVar exitResult)
                , withJWT userCreds
                ]
                ++ loggerOptions
          client <- newClient [(natsHost, natsPort)] clientOptions
          forkIO $ do
            outcome <- atomically $ (Left <$> readTMVar pinged) `orElse` (Right <$> readTMVar exitResult)
            case outcome of
              Left _  -> close client
              Right _ -> pure ()
          ping client (atomically (putTMVar pinged ()))
          result <- atomically $ readTMVar exitResult
          result `shouldBe` ExitClosedByUser
