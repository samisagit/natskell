{-# LANGUAGE OverloadedStrings #-}

module ConnectionSpec (spec) where

import           Auth.None                 (auth)
import qualified Client
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Data.IORef                (newIORef, readIORef, writeIORef)
import           Handshake.Nats
    ( HandshakeError (HandshakeAuthError, HandshakeTLSError, HandshakeTimeout)
    , performHandshake
    )
import           Lib.Logger
    ( LogLevel (Debug)
    , LoggerConfig (LoggerConfig)
    , newLogContext
    )
import           Network.Connection        (connectionApi)
import           Network.Connection.Core   (Transport (..), pointTransport)
import           Network.ConnectionAPI
    ( closeReader
    , newConn
    , readData
    , reader
    )
import           Parser.API
    ( ParseStep (DropPrefix, Emit, NeedMore, Reject)
    , ParsedMessage (ParsedInfo, ParsedPing, ParsedPong)
    , ParserAPI (ParserAPI)
    )
import qualified Parser.Attoparsec         as Attoparsec
import qualified Queue.API                 as Queue
import           Queue.TransactionalQueue  (newQueue)
import           Router.Nats
    ( RouteDirective (RouteContinue)
    , routeMessage
    )
import           State.Store
    ( newClientState
    , pushPingAction
    , queue
    , readServerInfo
    )
import           State.Types               (ClientConfig (..))
import           Subscription.Store        (newSubscriptionStore)
import           System.Timeout            (timeout)
import           Test.Hspec
import           Transformers.Transformers (Transformer (transform))
import qualified Types.Connect             as Connect
import           Types.Info                (Info (Info))
import           Types.Ping                (Ping (Ping))
import           Types.Pong                (Pong (Pong))
import           Types.TLS                 (TLSConfig (..), defaultTLSConfig)

spec :: Spec
spec = do
  describe "TLS configuration" $ do
    it "does not expose client private keys when rendered" $ do
      let config = defaultTLSConfig
            { tlsClientCertificate = Just ("certificate", "private-key")
            , tlsRootCertificates = ["private-root"]
            }

      show config `shouldNotContain` "private-key"
      show config `shouldNotContain` "private-root"
      show config `shouldContain` "tlsRootCertificates = 1 configured"

  describe "Connection reader" $ do
    it "unblocks a blocking read when closeReader is called" $ do
      conn <- newConn connectionApi
      started <- newEmptyMVar
      blocker <- (newEmptyMVar :: IO (MVar BS.ByteString))
      let transport = Transport
            { transportRead = \_ -> putMVar started () >> takeMVar blocker
            , transportWrite = \_ -> pure ()
            , transportWriteLazy = \_ -> pure ()
            , transportFlush = pure ()
            , transportClose = pure ()
            , transportUpgrade = Nothing
            }
      pointTransport conn transport
      resultVar <- newEmptyMVar
      _ <- forkIO $ readData (reader connectionApi) conn 1 >>= putMVar resultVar
      _ <- takeMVar started
      closeReader (reader connectionApi) conn
      result <- timeout 1000000 (takeMVar resultVar)
      result `shouldBe` Just (Left "Read operation is blocked")
  describe "Router ping lifecycle" $ do
    it "responds to server PING by enqueueing PONG" $ do
      state <- newTestState
      store <- newSubscriptionStore

      directive <- routeMessage state store (ParsedPing Ping)

      directive `shouldBe` RouteContinue
      queued <- Queue.dequeue (queue state)
      case queued of
        Right item ->
          LBS.toStrict (transform item) `shouldBe` "PONG\r\n"
        Left err ->
          expectationFailure ("expected queued PONG, got queue error: " ++ err)

    it "runs the next pending ping action when PONG is routed" $ do
      state <- newTestState
      store <- newSubscriptionStore
      actionRan <- newIORef False
      pushPingAction state (writeIORef actionRan True)

      directive <- routeMessage state store (ParsedPong Pong)

      directive `shouldBe` RouteContinue
      readIORef actionRan `shouldReturn` True
  describe "Handshake" $ do
    it "returns a typed error when no servers are configured" $ do
      result <- Client.newClient [] [Client.withConnectionAttempts 1]
      case result of
        Left Client.ConnectNoServers -> pure ()
        Left err -> expectationFailure ("unexpected connection error: " ++ show err)
        Right _ -> expectationFailure "client connected without a server"

    it "accepts an incremental parser backend for the initial INFO frame" $ do
      state <- newTestState
      conn <- newConn connectionApi
      writes <- newTVarIO []
      transport <-
        newScriptedTransport
          [ "INF"
          , "O {\"server_id\":\"srv\",\"version\":\"1.0.0\",\"go\":\"go1\",\"host\":\"127.0.0.1\",\"port\":4222,\"max_payload\":1024,\"proto\":1}\r\n"
          , "PONG\r\n"
          ]
          writes
      pointTransport conn transport

      result <- performHandshake connectionApi incrementalInfoParser state auth conn "127.0.0.1"

      result `shouldBe` Right ()
      readServerInfo state `shouldReturn` Just testInfo
      readTVarIO writes `shouldReturn`
        [LBS.toStrict (transform testConnect <> transform Ping)]

    it "accepts a parser backend that drops an invalid prefix before INFO" $ do
      state <- newTestState
      conn <- newConn connectionApi
      writes <- newTVarIO []
      transport <-
        newScriptedTransport
          [ "XIN"
          , "FO {\"server_id\":\"srv\",\"version\":\"1.0.0\",\"go\":\"go1\",\"host\":\"127.0.0.1\",\"port\":4222,\"max_payload\":1024,\"proto\":1}\r\n"
          , "PONG\r\n"
          ]
          writes
      pointTransport conn transport

      result <- performHandshake connectionApi dropPrefixInfoParser state auth conn "127.0.0.1"

      result `shouldBe` Right ()
      readServerInfo state `shouldReturn` Just testInfo
      readTVarIO writes `shouldReturn`
        [LBS.toStrict (transform testConnect <> transform Ping)]

    it "does not report readiness until the server sends PONG" $ do
      state <- newTestStateWithTimeout 20000
      conn <- newConn connectionApi
      writes <- newTVarIO []
      blocker <- newEmptyMVar
      let transport = Transport
            { transportRead = \_ -> do
                first <- atomically $ do
                  written <- readTVar writes
                  pure (null written)
                if first then pure infoFrame else takeMVar blocker
            , transportWrite = \bytes ->
                atomically $ modifyTVar' writes (<> [bytes])
            , transportWriteLazy = \bytes ->
                atomically $ modifyTVar' writes (<> [LBS.toStrict bytes])
            , transportFlush = pure ()
            , transportClose = pure ()
            , transportUpgrade = Nothing
            }
      pointTransport conn transport

      result <- performHandshake connectionApi incrementalInfoParser state auth conn "127.0.0.1"

      result `shouldBe` Left HandshakeTimeout

    it "accepts +OK before the handshake PONG" $ do
      state <- newTestState
      conn <- newConn connectionApi
      writes <- newTVarIO []
      transport <- newScriptedTransport [infoFrame, "+OK\r\nPONG\r\n"] writes
      pointTransport conn transport

      result <- performHandshake connectionApi Attoparsec.parserApi state auth conn "127.0.0.1"

      result `shouldBe` Right ()

    it "returns authentication errors received before PONG" $ do
      state <- newTestState
      conn <- newConn connectionApi
      writes <- newTVarIO []
      transport <- newScriptedTransport
        [infoFrame, "-ERR 'Authorization Violation'\r\n"] writes
      pointTransport conn transport

      result <- performHandshake connectionApi Attoparsec.parserApi state auth conn "127.0.0.1"

      case result of
        Left (HandshakeAuthError _) -> pure ()
        other -> expectationFailure ("expected auth error, got: " ++ show other)

    it "rejects requested TLS when the server does not advertise it" $ do
      state <- newTestStateWithConfig $ \cfg ->
        cfg { tlsConfig = Just defaultTLSConfig }
      conn <- newConn connectionApi
      writes <- newTVarIO []
      transport <- newScriptedTransport [infoFrame] writes
      pointTransport conn transport

      result <- performHandshake connectionApi Attoparsec.parserApi state auth conn "127.0.0.1"

      case result of
        Left (HandshakeTLSError _) -> pure ()
        other -> expectationFailure ("expected TLS error, got: " ++ show other)

newTestState = do
  newTestStateWithConfig id

newTestStateWithTimeout timeoutMicros = do
  newTestStateWithConfig $ \cfg -> cfg { connectTimeoutMicros = timeoutMicros }

newTestStateWithConfig updateConfig = do
  queue <- newQueue
  ctx <- newLogContext
  conn <- newConn connectionApi
  logger <- newSilentLogger
  newClientState (updateConfig (testConfig logger)) queue conn ctx

newSilentLogger :: IO LoggerConfig
newSilentLogger = do
  lock <- newTMVarIO ()
  pure (LoggerConfig Debug (\_ -> pure ()) lock)

testConfig :: LoggerConfig -> ClientConfig
testConfig logger =
  ClientConfig
    { connectionAttempts = 1
    , connectTimeoutMicros = 1000000
    , callbackConcurrency = 1
    , bufferLimit = 4096
    , connectConfig = testConnect
    , loggerConfig = logger
    , tlsConfig = Nothing
    , exitAction = const (pure ())
    , connectOptions = []
    }

testConnect :: Connect.Connect
testConnect =
  Connect.Connect
    { Connect.verbose = False
    , Connect.pedantic = True
    , Connect.tls_required = False
    , Connect.auth_token = Nothing
    , Connect.user = Nothing
    , Connect.pass = Nothing
    , Connect.name = Nothing
    , Connect.lang = "haskell"
    , Connect.version = "0.1.0"
    , Connect.protocol = Nothing
    , Connect.echo = Just True
    , Connect.sig = Nothing
    , Connect.jwt = Nothing
    , Connect.nkey = Nothing
    , Connect.no_responders = Just True
    , Connect.headers = Just True
    }

testInfo :: Info
testInfo =
  Info
    "srv"
    "1.0.0"
    "go1"
    "127.0.0.1"
    4222
    1024
    1
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

incrementalInfoParser :: ParserAPI ParsedMessage
incrementalInfoParser = ParserAPI parseInfo
  where
    parseInfo bytes
      | bytes == "INF" =
          NeedMore
      | bytes == infoFrame =
          Emit (ParsedInfo testInfo) ""
      | bytes == "PONG\r\n" =
          Emit (ParsedPong Pong) ""
      | otherwise =
          Reject ("unexpected input: " ++ show bytes)

dropPrefixInfoParser :: ParserAPI ParsedMessage
dropPrefixInfoParser = ParserAPI parseInfo
  where
    parseInfo bytes
      | bytes == "XIN" =
          DropPrefix 1 "invalid prefix"
      | bytes == infoFrame =
          Emit (ParsedInfo testInfo) ""
      | bytes == "PONG\r\n" =
          Emit (ParsedPong Pong) ""
      | otherwise =
          Reject ("unexpected input: " ++ show bytes)

infoFrame :: BS.ByteString
infoFrame =
  "INFO {\"server_id\":\"srv\",\"version\":\"1.0.0\",\"go\":\"go1\",\"host\":\"127.0.0.1\",\"port\":4222,\"max_payload\":1024,\"proto\":1}\r\n"

newScriptedTransport :: [BS.ByteString] -> TVar [BS.ByteString] -> IO Transport
newScriptedTransport chunks writes = do
  remainingChunks <- newTVarIO chunks
  pure $
    Transport
      { transportRead = \_ ->
          atomically $ do
            remaining <- readTVar remainingChunks
            case remaining of
              nextChunk:rest -> do
                writeTVar remainingChunks rest
                pure nextChunk
              [] ->
                pure BS.empty
      , transportWrite = \bytes ->
          atomically $ modifyTVar' writes (<> [bytes])
      , transportWriteLazy = \bytes ->
          atomically $ modifyTVar' writes (<> [LBS.toStrict bytes])
      , transportFlush = pure ()
      , transportClose = pure ()
      , transportUpgrade = Nothing
      }
