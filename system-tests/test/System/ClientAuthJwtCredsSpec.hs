{-# LANGUAGE OverloadedStrings #-}

module ClientAuthJwtCredsSpec (spec) where

import           Client
import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM
import           NatsServerConfig
import           Test.Hspec
import           TestSupport

spec :: Spec
spec =
  systemTest . describe "client auth" $ do
      logger <- runIO testLoggerConfig
      operatorJwt <- runIO (readFixtureText "jwt/operator.jwt")
      accountJwt <- runIO (readFixtureText "jwt/account.jwt")
      accountPub <- runIO (readFixtureText "jwt/account.pub")
      userCreds <- runIO (readFixtureBytesRaw "jwt/user.creds")
      let serverOptions =
            [ WithLogVerbosity NatsLogDebug
            , WithJwtConfig NatsJwtConfig
                { natsOperatorJwt = operatorJwt
                , natsSystemAccount = Nothing
                , natsJwtResolver = NatsJwtResolverMemory [(accountPub, accountJwt)]
                }
            ]
      around (withNatsContainerConfig serverOptions) $ do
        it "authenticates with jwt creds file" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOpts =
                [ withConnectName "auth-jwt-creds"
                , withExitAction (atomically . putTMVar exitResult)
                , withLoggerConfig logger
                , withJWT userCreds
                ]
          client <- newClient [(natsHost, natsPort)] clientOpts
          forkIO $ do
            outcome <- atomically $ (Left <$> readTMVar pinged) `orElse` (Right <$> readTMVar exitResult)
            case outcome of
              Left _  -> close client
              Right _ -> pure ()
          ping client (atomically (putTMVar pinged ()))
          result <- atomically $ readTMVar exitResult
          result `shouldBe` ExitClosedByUser
