{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module ConnectionSpec (spec) where

import           Auth.None                 (auth)
import qualified Client
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception         (finally, throwIO)
import           Control.Monad             (replicateM_, void, when)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Engine                    (runEngine)
import           Handshake.Nats
    ( HandshakeError (HandshakeAuthError, HandshakeProtocolError, HandshakeTLSError)
    , performHandshake
    )
import           Lib.Logger
    ( LogEntry (..)
    , LogLevel (Debug)
    , LoggerConfig (..)
    , newLogContext
    )
import           Network.Connection        (connectionApi)
import           Network.Connection.Core   (Transport (..), pointTransport)
import           Network.Connection.Tls    (receiveExactly)
import           Network.ConnectionAPI
    ( ConnectionAPI (..)
    , ReaderAPI (..)
    , WriterAPI (..)
    )
import           Parser.API
    ( ParseStep (DropPrefix, Emit, NeedMore, Reject)
    , ParsedMessage (ParsedErr, ParsedInfo, ParsedPing, ParsedPong)
    , ParserAPI (ParserAPI)
    )
import qualified Parser.Attoparsec         as Attoparsec
import           Pipeline.Broadcasting     (broadcastingApi)
import           Pipeline.Streaming        (streamingApi)
import qualified Queue.API                 as Queue
import           Queue.TransactionalQueue  (newQueue, newQueueWithCapacity)
import           Router.Nats
    ( RouteDirective (RouteContinue)
    , routeMessage
    )
import           State.Store
    ( PingResult (..)
    , PublishEnqueueResult (..)
    , ServerErrorEnqueueResult (..)
    , activateConnectionAttempt
    , beginConnectionAttempt
    , clearPendingPings
    , enqueueOnGenerationTracked
    , enqueuePublishOnConnected
    , enqueuePublishOnConnectedGenerationTracked
    , markClosed
    , markConnectionReady
    , newClientState
    , notifyServerError
    , queue
    , readConnectionGeneration
    , readServerInfo
    , readStatus
    , registerPingWaiter
    , registerPingWaiterAndEnqueue
    , setClosing
    , setReconnecting
    , setServerInfo
    , startCallbackWorker
    , stopCallbackWorker
    , waitForConnectionGenerationAfter
    , withSubscriptionGate
    )
import           State.Types
    ( ClientConfig (..)
    , ClientExitReason (..)
    , ConnectionEvent (..)
    , ConnectionState (..)
    , ServerErrorKind (..)
    , serverErrorFromProtocol
    , serverErrorKind
    )
import           Subscription.Store
    ( awaitCallbackDrain
    , enqueueControl
    , newSubscriptionStore
    , register
    , startWorkers
    )
import           Subscription.Types
    ( SubscribeConfig (..)
    , SubscriptionKind (StandardSubscription)
    , SubscriptionMeta (..)
    , defaultPendingLimits
    )
import           System.Timeout            (timeout)
import           Test.Hspec
import           Transformers.Transformers (Transformer (transform))
import qualified Types.Connect             as Connect
import qualified Types.Err                 as Err
import           Types.Info                (Info (..))
import           Types.Ping                (Ping (Ping))
import           Types.Pong                (Pong (Pong))
import qualified Types.Pub                 as Pub
import           Types.TLS                 (TLSConfig (..), defaultTLSConfig)

spec :: Spec
spec = do
  describe "Transactional queue" $ do
    it "wakes a full blocked enqueue when the queue closes" $ do
      messageQueue <- newQueueWithCapacity 1
      Queue.enqueue messageQueue (Queue.QueueItem Ping) `shouldReturn` Right ()
      started <- newEmptyMVar
      result <- newEmptyMVar
      _ <- forkIO $ do
        putMVar started ()
        Queue.enqueue messageQueue (Queue.QueueItem Ping) >>= putMVar result
      takeMVar started
      timeout 20000 (takeMVar result) `shouldReturn` Nothing

      Queue.close messageQueue

      timeout 1000000 (takeMVar result) `shouldReturn` Just (Left "Queue is closed")

    it "discards only connection-scoped commands at a connection boundary" $ do
      messageQueue <- newQueueWithCapacity 3
      Queue.enqueue messageQueue (Queue.QueueItem Ping) `shouldReturn` Right ()
      Queue.enqueue messageQueue (Queue.QueueConnectionScoped (Queue.QueueItem Ping))
        `shouldReturn` Right ()

      atomically (Queue.closeAndDiscardConnectionScoped messageQueue)

      Queue.dequeue messageQueue >>= \case
        Left err -> err `shouldBe` "Queue is closed"
        Right _  -> expectationFailure "expected closed queue"
      Queue.open messageQueue
      queued <- Queue.dequeue messageQueue
      case queued of
        Right item -> LBS.toStrict (transform item) `shouldBe` "PING\r\n"
        Left err   -> expectationFailure err
    it "discards every buffered command on terminal close" $ do
      messageQueue <- newQueueWithCapacity 2
      Queue.enqueue messageQueue (Queue.QueueItem Ping) `shouldReturn` Right ()
      Queue.enqueue messageQueue
        (Queue.QueueConnectionScoped (Queue.QueueItem Ping)) `shouldReturn` Right ()

      atomically (Queue.closeAndDiscardAll messageQueue)

      Queue.dequeue messageQueue >>= \case
        Left err -> err `shouldBe` "Queue is closed"
        Right _  -> expectationFailure "expected closed queue"
      Queue.open messageQueue
      blocked <- timeout 20000 (Queue.dequeue messageQueue)
      case blocked of
        Nothing -> pure ()
        Just _  -> expectationFailure "terminal close retained a buffered command"
      Queue.close messageQueue

    it "offers a nonblocking enqueue for cancellation cleanup" $ do
      messageQueue <- newQueueWithCapacity 1
      Queue.enqueue messageQueue (Queue.QueueItem Ping) `shouldReturn` Right ()

      timeout 1000000
        (Queue.tryEnqueue messageQueue (Queue.QueueItem Ping))
        `shouldReturn` Just Queue.TryQueueFull

  describe "TLS configuration" $ do
    it "does not expose client private keys when rendered" $ do
      let config = defaultTLSConfig
            { tlsClientCertificate = Just ("certificate", "private-key")
            , tlsRootCertificates = ["private-root"]
            }

      show config `shouldNotContain` "private-key"
      show config `shouldNotContain` "private-root"
      show config `shouldContain` "tlsRootCertificates = 1 configured"

    it "fills each TLS backend receive across partial socket reads" $ do
      chunks <- newMVar ["ab", "c", "de"]
      requested <- newMVar []
      let receive count = do
            modifyMVar_ requested (pure . (<> [count]))
            modifyMVar chunks $ \case
              []           -> pure ([], BS.empty)
              next : rest -> pure (rest, next)

      receiveExactly receive 5 `shouldReturn` "abcde"
      readMVar requested `shouldReturn` [5, 3, 2]

    it "returns a partial receive only when the socket reaches EOF" $ do
      chunks <- newMVar ["ab"]
      let receive _ =
            modifyMVar chunks $ \case
              []           -> pure ([], BS.empty)
              next : rest -> pure (rest, next)

      receiveExactly receive 5 `shouldReturn` "ab"

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
            , transportAbort = pure ()
            , transportUpgrade = Nothing
            }
      pointTransport conn transport
      resultVar <- newEmptyMVar
      _ <- forkIO $ readData (reader connectionApi) conn 1 >>= putMVar resultVar
      _ <- takeMVar started
      closeReader (reader connectionApi) conn
      result <- timeout 1000000 (takeMVar resultVar)
      result `shouldBe` Just (Left "Read operation is blocked")

    it "force-aborts a blocked transport write" $ do
      conn <- newConn connectionApi
      started <- newEmptyMVar
      release <- newEmptyMVar
      let transport = Transport
            { transportRead = const (pure BS.empty)
            , transportWrite = \_ -> putMVar started () >> takeMVar release
            , transportWriteLazy = \_ -> putMVar started () >> takeMVar release
            , transportFlush = pure ()
            , transportClose = pure ()
            , transportAbort = void (tryPutMVar release ())
            , transportUpgrade = Nothing
            }
      pointTransport conn transport
      resultVar <- newEmptyMVar
      _ <- forkIO $
        writeData (writer connectionApi) conn "blocked" >>= putMVar resultVar
      takeMVar started

      abort connectionApi conn

      timeout 1000000 (takeMVar resultVar) `shouldReturn` Just (Right ())
  describe "Router ping lifecycle" $ do
    it "responds to server PING by enqueueing PONG" $ do
      state <- newTestState
      store <- newSubscriptionStore defaultPendingLimits (pure ())
      markTestStateConnected state

      directive <- routeMessage state store (ParsedPing Ping)

      directive `shouldBe` RouteContinue
      queued <- Queue.dequeue (queue state)
      case queued of
        Right item ->
          LBS.toStrict (transform item) `shouldBe` "PONG\r\n"
        Left err ->
          expectationFailure ("expected queued PONG, got queue error: " ++ err)

    it "keeps routing when the configured logger throws" $ do
      state <- newTestStateWithConfig $ \cfg ->
        cfg
          { loggerConfig =
              (loggerConfig cfg)
                { logFn = const (throwIO (userError "logger failed")) }
          }
      store <- newSubscriptionStore defaultPendingLimits (pure ())
      markTestStateConnected state

      routeMessage state store (ParsedPing Ping) `shouldReturn` RouteContinue
      queued <- Queue.dequeue (queue state)
      case queued of
        Right item -> LBS.toStrict (transform item) `shouldBe` "PONG\r\n"
        Left err   -> expectationFailure err

    it "resolves exactly one pending ping waiter for each PONG" $ do
      state <- newTestState
      store <- newSubscriptionStore defaultPendingLimits (pure ())
      markTestStateConnected state
      firstWaiter <- newEmptyTMVarIO
      secondWaiter <- newEmptyTMVarIO
      registerPingWaiter state firstWaiter `shouldReturn` Right ()
      registerPingWaiter state secondWaiter `shouldReturn` Right ()

      firstDirective <- routeMessage state store (ParsedPong Pong)

      firstDirective `shouldBe` RouteContinue
      timeout 1000000 (atomically (readTMVar firstWaiter))
        `shouldReturn` Just PingReceived
      atomically (tryReadTMVar secondWaiter) `shouldReturn` Nothing

      secondDirective <- routeMessage state store (ParsedPong Pong)

      secondDirective `shouldBe` RouteContinue
      timeout 1000000 (atomically (readTMVar secondWaiter))
        `shouldReturn` Just PingReceived

    it "releases pending ping waiters at a connection boundary" $ do
      state <- newTestState
      markTestStateConnected state
      waiter <- newEmptyTMVarIO
      registerPingWaiter state waiter `shouldReturn` Right ()

      clearPendingPings state

      atomically (readTMVar waiter) `shouldReturn` PingConnectionLost

    it "atomically invalidates ping waiters and scoped PINGs on reconnect" $ do
      state <- newTestState
      markTestStateConnected state
      waiter <- newEmptyTMVarIO
      registerPingWaiter state waiter `shouldReturn` Right ()
      Queue.enqueue (queue state) (Queue.QueueConnectionScoped (Queue.QueueItem Ping))
        `shouldReturn` Right ()

      setReconnecting state

      atomically (readTMVar waiter) `shouldReturn` PingConnectionLost
      Queue.dequeue (queue state) >>= \case
        Left err -> err `shouldBe` "Queue is closed"
        Right _  -> expectationFailure "expected scoped PING to be discarded"

    it "does not orphan a ping waiter when reset wins a full-queue race" $ do
      state <- newTestStateWithQueueCapacity 1
      markTestStateConnected state
      Queue.enqueue (queue state) (Queue.QueueItem Ping) `shouldReturn` Right ()
      waiter <- newEmptyTMVarIO
      started <- newEmptyMVar
      result <- newEmptyMVar
      _ <- forkIO $ do
        putMVar started ()
        registerPingWaiterAndEnqueue
          state
          waiter
          (Queue.QueueConnectionScoped (Queue.QueueItem Ping))
          >>= putMVar result
      takeMVar started
      timeout 20000 (takeMVar result) `shouldReturn` Nothing

      _ <- setReconnecting state

      timeout 1000000 (takeMVar result)
        `shouldReturn` Just (Left ExitResetRequested)
      atomically (tryReadTMVar waiter) `shouldReturn` Nothing
      Queue.dequeue (queue state) >>= \case
        Left err -> err `shouldBe` "Queue is closed"
        Right _  -> expectationFailure "expected closed queue"

      Just attempt <- beginConnectionAttempt state
      activateConnectionAttempt state attempt `shouldReturn` True
      markConnectionReady state attempt `shouldReturn` True
      queued <- Queue.dequeue (queue state)
      case queued of
        Right item -> LBS.toStrict (transform item) `shouldBe` "PING\r\n"
        Left err   -> expectationFailure err

    it "revalidates a standalone publish before a generation-changing commit" $ do
      state <- newTestStateWithQueueCapacity 1
      setServerInfo state (testInfo { max_payload = 4096 })
      markTestStateConnected state
      Queue.enqueue (queue state) (Queue.QueueItem Ping) `shouldReturn` Right ()
      started <- newEmptyMVar
      result <- newEmptyMVar
      _ <- forkIO $ do
        putMVar started ()
        enqueuePublishOnConnected state 2048 oversizedPublish >>= putMVar result
      takeMVar started
      timeout 20000 (takeMVar result) `shouldReturn` Nothing

      _ <- setReconnecting state
      setServerInfo state (testInfo { max_payload = 1024 })
      Just attempt <- beginConnectionAttempt state
      activateConnectionAttempt state attempt `shouldReturn` True
      markConnectionReady state attempt `shouldReturn` True

      timeout 1000000 (takeMVar result)
        `shouldReturn` Just (PublishTooLarge 1024)

    it "revalidates a request publish before a generation-changing commit" $ do
      state <- newTestStateWithQueueCapacity 1
      setServerInfo state (testInfo { max_payload = 4096 })
      markTestStateConnected state
      Queue.enqueue (queue state) (Queue.QueueItem Ping) `shouldReturn` Right ()
      committedGeneration <- newEmptyTMVarIO
      started <- newEmptyMVar
      result <- newEmptyMVar
      _ <- forkIO $ do
        putMVar started ()
        enqueuePublishOnConnectedGenerationTracked
          state
          committedGeneration
          2048
          (Queue.QueueConnectionScoped oversizedPublish)
          >>= putMVar result
      takeMVar started
      timeout 20000 (takeMVar result) `shouldReturn` Nothing

      _ <- setReconnecting state
      setServerInfo state (testInfo { max_payload = 1024 })
      Just attempt <- beginConnectionAttempt state
      activateConnectionAttempt state attempt `shouldReturn` True
      markConnectionReady state attempt `shouldReturn` True

      timeout 1000000 (takeMVar result)
        `shouldReturn` Just (PublishTooLarge 1024)
      atomically (tryReadTMVar committedGeneration) `shouldReturn` Nothing

    it "drops durable publishes that exceed a reconnect target's limit" $ do
      state <- newTestStateWithQueueCapacity 2
      rejected <- newEmptyMVar
      setServerInfo state (testInfo { max_payload = 4096 })
      markTestStateConnected state
      Queue.enqueue (queue state) (oversizedPublishWith (putMVar rejected))
        `shouldReturn` Right ()
      Queue.enqueue (queue state) (Queue.QueueItem Ping) `shouldReturn` Right ()

      _ <- setReconnecting state
      setServerInfo state (testInfo { max_payload = 1024 })
      Just attempt <- beginConnectionAttempt state
      activateConnectionAttempt state attempt `shouldReturn` True
      markConnectionReady state attempt `shouldReturn` True
      takeMVar rejected `shouldReturn` 1024

      Queue.dequeue (queue state) >>= \case
        Right item -> LBS.toStrict (transform item) `shouldBe` "PING\r\n"
        Left err   -> expectationFailure err
      remaining <- timeout 20000 (Queue.dequeue (queue state))
      case remaining of
        Nothing -> pure ()
        Just _  -> expectationFailure "oversized durable publish remained queued"

    it "dispatches durable publish rejection without blocking readiness" $ do
      state <- newTestStateWithQueueCapacity 1
      store <- newSubscriptionStore defaultPendingLimits (pure ())
      stopping <- newTVarIO False
      callbackStarted <- newEmptyMVar
      releaseCallback <- newEmptyMVar
      _ <- startWorkers
        1
        store
        (readTVar stopping >>= check)
        (const (pure ()))
      let handleRejection maximumSize = do
            putMVar callbackStarted maximumSize
            takeMVar releaseCallback

      setServerInfo state (testInfo { max_payload = 4096 })
      markTestStateConnected state
      Queue.enqueue
        (queue state)
        (oversizedPublishWith (enqueueControl store . handleRejection))
        `shouldReturn` Right ()

      _ <- setReconnecting state
      setServerInfo state (testInfo { max_payload = 1024 })
      Just attempt <- beginConnectionAttempt state
      activateConnectionAttempt state attempt `shouldReturn` True
      ready <- newEmptyMVar
      void . forkIO $ markConnectionReady state attempt >>= putMVar ready

      takeMVar callbackStarted `shouldReturn` 1024
      timeout 100000 (takeMVar ready) `shouldReturn` Just True
      putMVar releaseCallback ()
      atomically (awaitCallbackDrain store)
      atomically (writeTVar stopping True)

  describe "Connection lifecycle" $ do
    it "keeps the outbound queue closed until the attempt is ready" $ do
      state <- newTestState
      Just attempt <- beginConnectionAttempt state

      activateConnectionAttempt state attempt `shouldReturn` True
      Queue.enqueue (queue state) (Queue.QueueItem Ping)
        `shouldReturn` Left "Queue is closed"

      markConnectionReady state attempt `shouldReturn` True
      Queue.enqueue (queue state) (Queue.QueueItem Ping) `shouldReturn` Right ()

    it "rejects readiness from an attempt invalidated by reset" $ do
      state <- newTestState
      Just attempt <- beginConnectionAttempt state

      setReconnecting state
      activateConnectionAttempt state attempt `shouldReturn` False
      markConnectionReady state attempt `shouldReturn` False

      readStatus state `shouldReturn` ConnectionReconnecting

    it "does not overwrite a closing state with readiness" $ do
      state <- newTestState
      Just attempt <- beginConnectionAttempt state

      setClosing state ExitClosedByUser
      markConnectionReady state attempt `shouldReturn` False

      readStatus state `shouldReturn` ConnectionClosing ExitClosedByUser

    it "does not reopen the outbound queue after closing starts" $ do
      state <- newTestState
      setClosing state ExitClosedByUser

      beginConnectionAttempt state `shouldReturn` Nothing
      Queue.enqueue (queue state) (Queue.QueueItem Ping)
        `shouldReturn` Left "Queue is closed"

    it "waits for a connected generation strictly after the invalidated one" $ do
      state <- newTestState
      markTestStateConnected state
      invalidated <- setReconnecting state
      Just secondAttempt <- beginConnectionAttempt state
      activateConnectionAttempt state secondAttempt `shouldReturn` True
      markConnectionReady state secondAttempt `shouldReturn` True
      _ <- setReconnecting state

      timeout 20000
        (atomically (waitForConnectionGenerationAfter state invalidated))
        `shouldReturn` Nothing

      Just thirdAttempt <- beginConnectionAttempt state
      activateConnectionAttempt state thirdAttempt `shouldReturn` True
      markConnectionReady state thirdAttempt `shouldReturn` True
      timeout 1000000
        (atomically (waitForConnectionGenerationAfter state invalidated))
        `shouldReturn` Just ()

    it "keeps subscription operations behind the readiness transition" $ do
      state <- newTestState
      markTestStateConnected state
      _ <- setReconnecting state
      Just attempt <- beginConnectionAttempt state
      activateConnectionAttempt state attempt `shouldReturn` True
      callbackStarted <- newEmptyMVar
      callbackStatus <- newEmptyMVar

      withSubscriptionGate state $ do
        _ <- forkIO $ do
          putMVar callbackStarted ()
          withSubscriptionGate state (readStatus state >>= putMVar callbackStatus)
        takeMVar callbackStarted
        timeout 20000 (takeMVar callbackStatus) `shouldReturn` Nothing
        markConnectionReady state attempt `shouldReturn` True

      timeout 1000000 (takeMVar callbackStatus)
        `shouldReturn` Just ConnectionConnected

    it "rejects a stale subscription enqueue without holding readiness" $ do
      state <- newTestState
      markTestStateConnected state
      committedGeneration <- newEmptyTMVarIO
      readinessStarted <- newEmptyMVar
      readinessResult <- newEmptyMVar

      withSubscriptionGate state $ do
        generation <- readConnectionGeneration state
        _ <- setReconnecting state
        Just attempt <- beginConnectionAttempt state
        activateConnectionAttempt state attempt `shouldReturn` True
        void . forkIO $ do
          putMVar readinessStarted ()
          withSubscriptionGate state (markConnectionReady state attempt)
            >>= putMVar readinessResult
        takeMVar readinessStarted
        timeout 20000 (takeMVar readinessResult) `shouldReturn` Nothing

        enqueueOnGenerationTracked
          state
          generation
          committedGeneration
          (Queue.QueueConnectionScoped (Queue.QueueItem Ping))
          `shouldReturn` Left ExitResetRequested
        atomically (tryReadTMVar committedGeneration) `shouldReturn` Nothing

      timeout 1000000 (takeMVar readinessResult) `shouldReturn` Just True
      readStatus state `shouldReturn` ConnectionConnected

    it "serializes a delayed pre-ready PONG with runtime writes" $ do
      preReadyPing <- newEmptyMVar
      loggerLock <- newTMVarIO ()
      let logger = LoggerConfig Debug (\entry ->
            when (leMessage entry == "routing pre-ready PING") $
              void (tryPutMVar preReadyPing ())) loggerLock
          cfg =
            (testConfig logger)
              { connectionAttempts = 1
              , connectTimeoutMicros = 2 * 1000000
              , connectOptions = [("fake", 4222)]
              }
      messageQueue <- newQueue
      ctx <- newLogContext
      conn <- newConn connectionApi
      state <- newClientState cfg messageQueue conn ctx
      store <- newSubscriptionStore defaultPendingLimits (pure ())
      markTestStateConnected state
      Queue.enqueue
        (queue state)
        (Queue.QueueItem (Pub.Pub "RUNTIME" Nothing Nothing (Just "x")))
        `shouldReturn` Right ()
      _ <- setReconnecting state
      register
        store
        "1"
        (SubscriptionMeta "READY" Nothing StandardSubscription)
        (SubscribeConfig Nothing Nothing)
        (const (pure ()))
        (pure ())
      reads <- newTQueueIO
      atomically $ do
        writeTQueue reads infoFrame
        writeTQueue reads "PONG\r\n"
      resubscribeStarted <- newEmptyMVar
      releaseResubscribe <- newEmptyMVar
      pongStarted <- newEmptyMVar
      releasePong <- newEmptyMVar
      runtimeWriteStarted <- newEmptyMVar
      let writeFrame bytes
            | "CONNECT " `BS.isInfixOf` bytes = pure ()
            | "SUB READY 1\r\n" `BS.isInfixOf` bytes = do
                atomically (writeTQueue reads "PING\r\n")
                putMVar resubscribeStarted ()
                takeMVar releaseResubscribe
            | bytes == "PONG\r\n" = do
                putMVar pongStarted ()
                takeMVar releasePong
            | "PUB RUNTIME " `BS.isInfixOf` bytes =
                putMVar runtimeWriteStarted ()
            | otherwise = pure ()
          transport = Transport
            { transportRead = const (atomically (readTQueue reads))
            , transportWrite = writeFrame
            , transportWriteLazy = writeFrame . LBS.toStrict
            , transportFlush = pure ()
            , transportClose = pure ()
            , transportAbort = do
                void (tryPutMVar releaseResubscribe ())
                void (tryPutMVar releasePong ())
                atomically (writeTQueue reads BS.empty)
            , transportUpgrade = Nothing
            }
          fakeConnectionApi = connectionApi
            { connectTcp = \target _ _ -> do
                pointTransport target transport
                pure (Right ())
            , configure = \_ _ -> pure (Right ())
            }
      engineDone <- newEmptyMVar
      _ <- forkIO $
        runEngine
          fakeConnectionApi
          streamingApi
          broadcastingApi
          Attoparsec.parserApi
          state
          store
          auth
          `finally` putMVar engineDone ()

      timeout 1000000 (takeMVar resubscribeStarted) `shouldReturn` Just ()
      timeout 1000000 (takeMVar preReadyPing) `shouldReturn` Just ()
      putMVar releaseResubscribe ()
      timeout 1000000 (takeMVar pongStarted) `shouldReturn` Just ()
      timeout 20000 (takeMVar runtimeWriteStarted) `shouldReturn` Nothing
      putMVar releasePong ()
      timeout 1000000 (takeMVar runtimeWriteStarted) `shouldReturn` Just ()

      setClosing state ExitClosedByUser
      abort fakeConnectionApi conn
      timeout 1000000 (takeMVar engineDone) `shouldReturn` Just ()

    it "applies the attempt deadline to stalled resubscription writes" $ do
      messageQueue <- newQueue
      ctx <- newLogContext
      conn <- newConn connectionApi
      logger <- newSilentLogger
      let cfg =
            (testConfig logger)
              { connectionAttempts = 1
              , connectTimeoutMicros = 3000000
              , connectOptions = [("fake", 4222)]
              }
      state <- newClientState cfg messageQueue conn ctx
      store <- newSubscriptionStore defaultPendingLimits (pure ())
      markTestStateConnected state
      _ <- setReconnecting state
      register
        store
        "1"
        (SubscriptionMeta "READY" Nothing StandardSubscription)
        (SubscribeConfig Nothing Nothing)
        (const (pure ()))
        (pure ())
      reads <- newTQueueIO
      atomically $ do
        writeTQueue reads infoFrame
        writeTQueue reads "PONG\r\n"
      resubscribeStarted <- newEmptyMVar
      releaseWrite <- newEmptyMVar
      let transport = Transport
            { transportRead = const (atomically (readTQueue reads))
            , transportWrite = const (pure ())
            , transportWriteLazy = \bytes ->
                when ("SUB READY 1\r\n" `BS.isInfixOf` LBS.toStrict bytes) $ do
                  putMVar resubscribeStarted ()
                  takeMVar releaseWrite
            , transportFlush = pure ()
            , transportClose = pure ()
            , transportAbort = do
                void (tryPutMVar releaseWrite ())
                atomically (writeTQueue reads BS.empty)
            , transportUpgrade = Nothing
            }
          fakeConnectionApi = connectionApi
            { connectTcp = \target _ _ -> do
                threadDelay 1500000
                pointTransport target transport
                pure (Right ())
            , configure = \_ _ -> pure (Right ())
            }
      engineDone <- newEmptyMVar
      engineThread <- forkIO $
        runEngine
          fakeConnectionApi
          streamingApi
          broadcastingApi
          Attoparsec.parserApi
          state
          store
          auth
          `finally` putMVar engineDone ()
      let stopFailedEngine = do
            void (tryPutMVar releaseWrite ())
            setClosing state ExitClosedByUser
            abort fakeConnectionApi conn
            killThread engineThread
            readMVar engineDone

      started <- timeout 5000000 (takeMVar resubscribeStarted)
      case started of
        Just () -> pure ()
        Nothing -> do
          status <- readStatus state
          stopFailedEngine
          expectationFailure
            ("resubscription did not start; status=" ++ show status)
      completed <- timeout 2250000 (readMVar engineDone)
      case completed of
        Just () -> pure ()
        Nothing -> do
          status <- readStatus state
          stopFailedEngine
          expectationFailure
            ("engine exceeded the shared attempt deadline; status=" ++ show status)
      readStatus state >>= \case
        ConnectionClosed _ -> pure ()
        status -> expectationFailure
          ("engine remained live after readiness deadline: " ++ show status)

  describe "Server errors" $ do
    it "classifies typed protocol errors without parsing their reason" $ do
      serverErrorKind (serverErrorFromProtocol (Err.ErrAuthViolation "secret"))
        `shouldBe` ServerErrorAuthentication
      serverErrorKind (serverErrorFromProtocol (Err.ErrPermViolation "private.subject"))
        `shouldBe` ServerErrorPermission
      serverErrorKind (serverErrorFromProtocol (Err.ErrInvalidProtocol "bad"))
        `shouldBe` ServerErrorProtocol
      serverErrorKind (serverErrorFromProtocol (Err.ErrErr "future server error"))
        `shouldBe` ServerErrorUnknown

    it "retains the legacy ServerError Show representation" $ do
      let serverError = serverErrorFromProtocol (Err.ErrPermViolation "private")
      show serverError
        `shouldBe` "ServerError \"private\""
      show (ExitServerError serverError)
        `shouldBe` "ExitServerError (ServerError \"private\")"
      show (ConnectionEventClosed (ExitServerError serverError))
        `shouldBe` "ConnectionEventClosed (ExitServerError (ServerError \"private\"))"

    it "bounds handler events without blocking the protocol producer" $ do
      state <- newTestState
      let serverError = serverErrorFromProtocol (Err.ErrPermViolation "private")

      replicateM_ 256
        (notifyServerError state serverError `shouldReturn` ServerErrorQueued)

      notifyServerError state serverError `shouldReturn` ServerErrorDroppedReport
      notifyServerError state serverError `shouldReturn` ServerErrorDropped

    it "bounds handler events by retained reason bytes" $ do
      state <- newTestState
      let largeReason = BS.replicate (1024 * 1024) 120
          serverError = serverErrorFromProtocol (Err.ErrPermViolation largeReason)

      notifyServerError state serverError `shouldReturn` ServerErrorQueued
      notifyServerError state serverError `shouldReturn` ServerErrorDroppedReport

    it "drops nonfatal errors instead of reconnecting when callbacks are full" $ do
      state <- newTestState
      store <- newSubscriptionStore defaultPendingLimits (pure ())
      let protocolError = Err.ErrPermViolation "private"
          serverError = serverErrorFromProtocol protocolError
      replicateM_ 256
        (notifyServerError state serverError `shouldReturn` ServerErrorQueued)

      routeMessage state store (ParsedErr protocolError)
        `shouldReturn` RouteContinue

    it "guarantees lifecycle callback order when server callbacks are full" $ do
      observed <- newTQueueIO
      state <- newTestStateWithConfig $ \cfg ->
        cfg { connectionEventHandler = atomically . writeTQueue observed }
      let serverError = serverErrorFromProtocol (Err.ErrPermViolation "private")
      replicateM_ 256
        (notifyServerError state serverError `shouldReturn` ServerErrorQueued)
      notifyServerError state serverError `shouldReturn` ServerErrorDroppedReport
      markTestStateConnected state
      setReconnecting state
      Just attempt <- beginConnectionAttempt state
      activateConnectionAttempt state attempt `shouldReturn` True
      markConnectionReady state attempt `shouldReturn` True
      setClosing state ExitClosedByUser
      markClosed state ExitClosedByUser `shouldReturn` Just ExitClosedByUser
      eventWorker <- startCallbackWorker state

      atomically (readTQueue observed)
        `shouldReturn` ConnectionEventDisconnected
      atomically (readTQueue observed)
        `shouldReturn` ConnectionEventReconnected
      atomically (readTQueue observed)
        `shouldReturn` ConnectionEventClosed ExitClosedByUser
      stopCallbackWorker eventWorker
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

    it "accepts normally chunked INFO and PONG frames" $ do
      state <- newTestState
      conn <- newConn connectionApi
      writes <- newTVarIO []
      let (infoStart, infoEnd) = BS.splitAt 40 infoFrame
      transport <- newScriptedTransport [infoStart, infoEnd, "PO", "NG\r\n"] writes
      pointTransport conn transport

      result <- performHandshake connectionApi Attoparsec.parserApi state auth conn "127.0.0.1"

      result `shouldBe` Right ()

    it "rejects an oversized incomplete INFO control frame" $ do
      state <- newTestState
      conn <- newConn connectionApi
      writes <- newTVarIO []
      transport <- newScriptedTransport oversizedHandshakeChunks writes
      pointTransport conn transport

      result <- performHandshake connectionApi stalledInfoParser state auth conn "127.0.0.1"

      result `shouldBe`
        Left (HandshakeProtocolError "INFO control frame exceeds 65536 byte limit")

    it "rejects an oversized incomplete control frame before PONG" $ do
      state <- newTestState
      conn <- newConn connectionApi
      writes <- newTVarIO []
      transport <- newScriptedTransport (infoFrame : oversizedHandshakeChunks) writes
      pointTransport conn transport

      result <- performHandshake connectionApi stalledPongParser state auth conn "127.0.0.1"

      result `shouldBe`
        Left (HandshakeProtocolError "PONG control frame exceeds 65536 byte limit")

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
            , transportAbort = pure ()
            , transportUpgrade = Nothing
            }
      pointTransport conn transport

      result <- timeout 20000 $
        performHandshake connectionApi incrementalInfoParser state auth conn "127.0.0.1"

      result `shouldBe` Nothing
      putMVar blocker BS.empty

    it "accepts +OK before the handshake PONG" $ do
      state <- newTestState
      conn <- newConn connectionApi
      writes <- newTVarIO []
      transport <- newScriptedTransport [infoFrame, "+OK\r\nPONG\r\n"] writes
      pointTransport conn transport

      result <- performHandshake connectionApi Attoparsec.parserApi state auth conn "127.0.0.1"

      result `shouldBe` Right ()

    it "preserves protocol bytes read after the handshake PONG" $ do
      state <- newTestState
      conn <- newConn connectionApi
      writes <- newTVarIO []
      transport <- newScriptedTransport
        [infoFrame, "PONG\r\nMSG READY 1 2\r\nok\r\n"] writes
      pointTransport conn transport

      result <- performHandshake
        connectionApi Attoparsec.parserApi state auth conn "127.0.0.1"

      result `shouldBe` Right ()
      readData (reader connectionApi) conn 4096
        `shouldReturn` Right "MSG READY 1 2\r\nok\r\n"

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
  newTestStateWithQueue updateConfig queue

newTestStateWithQueueCapacity capacity = do
  queue <- newQueueWithCapacity capacity
  newTestStateWithQueue id queue

newTestStateWithQueue updateConfig queue = do
  ctx <- newLogContext
  conn <- newConn connectionApi
  logger <- newSilentLogger
  newClientState (updateConfig (testConfig logger)) queue conn ctx

markTestStateConnected state = do
  Just attempt <- beginConnectionAttempt state
  activateConnectionAttempt state attempt `shouldReturn` True
  markConnectionReady state attempt `shouldReturn` True

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
    , messageLimit = 1024 * 1024
    , connectConfig = testConnect
    , loggerConfig = logger
    , tlsConfig = Nothing
    , serverErrorHandler = const (pure ())
    , connectionEventHandler = const (pure ())
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

oversizedPublish :: Queue.QueueItem
oversizedPublish = oversizedPublishWith (const (pure ()))

oversizedPublishWith :: (Int -> IO ()) -> Queue.QueueItem
oversizedPublishWith reject =
  Queue.QueuePayloadBound 2048 reject . Queue.QueueItem $
    Pub.Pub
      { Pub.subject = "BOUNDARY.TOO_BIG"
      , Pub.payload = Just (BS.replicate 2048 120)
      , Pub.replyTo = Nothing
      , Pub.headers = Nothing
      }

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
      | bytes == "IN" =
          NeedMore
      | bytes == infoFrame =
          Emit (ParsedInfo testInfo) ""
      | bytes == "PONG\r\n" =
          Emit (ParsedPong Pong) ""
      | otherwise =
          Reject ("unexpected input: " ++ show bytes)

stalledInfoParser :: ParserAPI ParsedMessage
stalledInfoParser = ParserAPI (const NeedMore)

stalledPongParser :: ParserAPI ParsedMessage
stalledPongParser = ParserAPI parseFrame
  where
    parseFrame bytes
      | bytes == infoFrame = Emit (ParsedInfo testInfo) ""
      | otherwise = NeedMore

oversizedHandshakeChunks :: [BS.ByteString]
oversizedHandshakeChunks = replicate 17 (BS.replicate 4096 73)

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
      , transportAbort = pure ()
      , transportUpgrade = Nothing
      }
