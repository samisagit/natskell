module Engine
  ( closeClient
  , interruptConnection
  , resetClient
  , runEngine
  ) where

import           Auth.Types
import           Control.Concurrent
    ( MVar
    , ThreadId
    , forkIOWithUnmask
    , killThread
    , myThreadId
    , newMVar
    , withMVar
    )
import           Control.Concurrent.STM
import           Control.Exception
    ( SomeException
    , finally
    , mask
    , mask_
    , onException
    )
import           Control.Monad             (unless, void, when)
import           Data.Foldable             (for_)
import           Data.Maybe                (listToMaybe)
import           GHC.Clock                 (getMonotonicTimeNSec)
import           Handshake.Nats
    ( HandshakeError (..)
    , performHandshake
    )
import           Lib.Exception             (trySync)
import           Lib.Logger                (LogLevel (..), MonadLogger (..))
import           Network.ConnectionAPI
    ( Conn
    , ConnectionAPI
    , WriterAPI (..)
    , abort
    , close
    , closeReader
    , connectTcp
    , open
    , reader
    , writer
    )
import           Parser.API
    ( ParsedMessage (ParsedPing)
    , ParserAPI
    )
import           Pipeline.Broadcasting.API (BroadcastingAPI (BroadcastingAPI))
import           Pipeline.Streaming.API    (StreamingAPI (StreamingAPI))
import           Queue.API                 (QueueItem (QueueBatch, QueueItem))
import           Router.Nats               (RouteDirective (..), routeMessage)
import           State.Store
    ( ClientState
    , activateConnectionAttempt
    , beginConnectionAttempt
    , closeQueue
    , config
    , connectedOnce
    , connection
    , failInitialConnection
    , incrementAttemptIndex
    , isManagedThread
    , markClosed
    , markConnectionReadyWithRejectedPublishes
    , queue
    , readAttemptIndex
    , readStatus
    , registerManagedThread
    , runClient
    , setClosing
    , setEndpoint
    , setReconnecting
    , unregisterManagedThread
    , waitForClosed
    , waitForConnectionGenerationAfter
    , withSubscriptionGate
    )
import           State.Types
    ( ClientConfig (..)
    , ClientExitReason (..)
    , ConnectAttemptError (..)
    , ConnectError (..)
    , ConnectFailure (..)
    , ConnectionState (..)
    , serverErrorKind
    )
import           Subscription.Store
    ( SubscriptionStore
    , active
    , closeStore
    )
import           Subscription.Types
    ( SubscriptionMeta (SubscriptionMeta)
    , isOneShotSubscription
    , isResumableSubscription
    )
import           System.Timeout            (timeout)
import           Transformers.Transformers (Transformer (transform))
import           Types.Pong                (Pong (Pong))
import qualified Types.Sub                 as Sub
import qualified Types.Unsub               as Unsub

data ConnectionRunResult = ConnectionDisconnected
                         | ConnectionExit ClientExitReason

data ReaderRuntime = ReaderRuntime
                       { runtimeReaderThread :: ThreadId
                       , runtimeReaderDone   :: TMVar ()
                       , runtimeExit         :: TMVar ClientExitReason
                       }

runEngine
  :: ConnectionAPI
  -> StreamingAPI
  -> BroadcastingAPI
  -> ParserAPI ParsedMessage
  -> ClientState
  -> SubscriptionStore
  -> Auth
  -> IO ()
runEngine connectionApi streamingApi broadcastingApi parserApi state store auth =
  mask $ \restore -> do
    result <- trySync (restore engine `finally` cleanupEngineResources)
    case result of
      Left err -> handleEngineFailure err
      Right () -> pure ()
  where
    StreamingAPI runStreaming = streamingApi
    BroadcastingAPI runBroadcasting = broadcastingApi

    engine =
      case connectOptions (config state) of
        [] -> do
          failInitialConnection state ConnectNoServers
          finalize (ExitRetriesExhausted (Just "No servers provided"))
        _ ->
          loop (connectionAttempts (config state)) []

    handleEngineFailure :: SomeException -> IO ()
    handleEngineFailure _ = mask_ $ do
      let reason = ExitRetriesExhausted (Just "client engine failed")
      failInitialConnection state (ConnectAttemptsExhausted [])
      setClosing state reason
      finalize reason

    cleanupEngineResources = mask_ $ do
      let ignoreSync action = void (trySync action)
          conn = connection state
      ignoreSync (closeQueue state)
      ignoreSync (closeReader (reader connectionApi) conn)
      ignoreSync (closeWriter (writer connectionApi) conn)
      ignoreSync (abort connectionApi conn)
      ignoreSync (close connectionApi conn)

    loop remaining attemptErrors
      | remaining <= 0 = do
          runClient state $
            logMessage Info "retries exhausted; exiting"
          let orderedErrors = reverse attemptErrors
              connectError = ConnectAttemptsExhausted orderedErrors
              lastErr = show <$> listToMaybe orderedErrors
          failInitialConnection state connectError
          finalize (ExitRetriesExhausted lastErr)
      | otherwise = do
          status <- readStatus state
          case status of
            ConnectionConnecting -> continueLoop remaining attemptErrors
            ConnectionConnected -> continueLoop remaining attemptErrors
            ConnectionReconnecting -> continueLoop remaining attemptErrors
            ConnectionClosing reason ->
              finalize reason
            ConnectionClosed _ ->
              pure ()
      where
        continueLoop remaining' attemptErrors' = do
          attemptErr <- runAttempt
          nextStatus <- readStatus state
          case nextStatus of
            ConnectionConnecting -> retryLoop attemptErr
            ConnectionConnected -> retryLoop attemptErr
            ConnectionReconnecting -> retryLoop attemptErr
            ConnectionClosing reason ->
              finalize reason
            ConnectionClosed _ ->
              pure ()
          where
            retryLoop attemptErr = do
              when (remaining' > 1) $ do
                runClient state $
                  logMessage Info "retrying client connection"
                incrementAttemptIndex state
              loop (remaining' - 1) (attemptErr : attemptErrors')

    runAttempt = do
      let cfg = config state
          endpoints = connectOptions cfg
      attemptIndex <- readAttemptIndex state
      let endpoint@(host, _port) =
            endpoints !! (attemptIndex `mod` length endpoints)
          attemptTimeout = max 1 (connectTimeoutMicros cfg)
      attempt <- beginConnectionAttempt state
      case attempt of
        Nothing ->
          pure (ConnectAttemptError endpoint (ConnectTransportFailure "connection attempt cancelled"))
        Just attemptEpoch -> do
          attemptStarted <- toInteger <$> getMonotonicTimeNSec
          setEndpoint state endpoint
          acquired <- timeout attemptTimeout (acquireTransport endpoint)
          case acquired of
            Nothing -> do
              runClient state $
                logMessage Error "transport acquisition timed out"
              terminateTransport (connection state)
              pure (ConnectAttemptError endpoint (ConnectTransportFailure "transport acquisition timed out"))
            Just (Left err) -> do
              terminateTransport (connection state)
              pure (ConnectAttemptError endpoint (ConnectTransportFailure err))
            Just (Right conn) -> do
              open connectionApi conn
              activated <- activateConnectionAttempt state attemptEpoch
              if not activated
                then do
                  terminateTransport conn
                  pure (ConnectAttemptError endpoint (ConnectTransportFailure "connection attempt cancelled"))
                else do
                  remaining <- remainingAttemptMicros attemptStarted attemptTimeout
                  initialized <-
                    if remaining <= 0
                      then pure Nothing
                      else timeout remaining $
                        performHandshake connectionApi parserApi state auth conn host
                  case initialized of
                    Nothing -> do
                      runClient state $
                        logMessage Error "connection handshake timed out"
                      terminateTransport conn
                      pure (ConnectAttemptError endpoint ConnectHandshakeTimeout)
                    Just (Left handshakeError) -> do
                      let failure = handshakeFailure handshakeError
                      runClient state $
                        logMessage Error ("connection initialization failed: " ++ failureCategory failure)
                      terminateTransport conn
                      pure (ConnectAttemptError endpoint failure)
                    Just (Right ()) ->
                      finishConnection
                        endpoint
                        conn
                        attemptEpoch
                        attemptStarted
                        attemptTimeout

    finishConnection endpoint conn attemptEpoch attemptStarted attemptTimeout = mask $ \restore -> do
      writeLock <- newMVar ()
      readerRuntime <- startConnectionReader conn writeLock
      remaining <- remainingAttemptMicros attemptStarted attemptTimeout
      readinessResult <-
        if remaining <= 0
          then pure Nothing
          else restore (timeout remaining (completeReadiness writeLock conn attemptEpoch))
            `onException` terminateReader conn readerRuntime
      case readinessResult of
        Nothing -> do
          runClient state $
            logMessage Error "connection readiness timed out"
          terminateReader conn readerRuntime
          terminateTransport conn
          pure
            ( ConnectAttemptError
                endpoint
                (ConnectTransportFailure "connection readiness timed out")
            )
        Just (Left err) -> do
          runClient state $
            logMessage Error
              ("resubscription failed: " ++ failureCategory (ConnectTransportFailure err))
          void (setReconnecting state)
          terminateReader conn readerRuntime
          terminateTransport conn
          pure (ConnectAttemptError endpoint (ConnectTransportFailure err))
        Just (Right False) -> do
          closeQueue state
          terminateReader conn readerRuntime
          terminateTransport conn
          pure (ConnectAttemptError endpoint (ConnectTransportFailure "connection attempt cancelled"))
        Just (Right True) -> do
          connectionResult <- restore (runConnection conn writeLock readerRuntime)
            `onException` terminateReader conn readerRuntime
          case connectionResult of
            ConnectionDisconnected -> void (setReconnecting state)
            ConnectionExit reason  -> setClosing state reason
          close connectionApi conn
          case connectionResult of
            ConnectionDisconnected -> do
              runClient state $
                logMessage Info "connection disconnected"
              pure (ConnectAttemptError endpoint (ConnectTransportFailure "connection disconnected"))
            ConnectionExit reason ->
              pure (ConnectAttemptError endpoint (ConnectProtocolFailure (exitCategory reason)))

    terminateTransport conn = do
      abort connectionApi conn
      close connectionApi conn

    remainingAttemptMicros started timeoutMicros = do
      now <- toInteger <$> getMonotonicTimeNSec
      let deadline = started + toInteger timeoutMicros * 1000
          remainingNanos = max 0 (deadline - now)
          roundedMicros = (remainingNanos + 999) `div` 1000
      pure (fromInteger (min (toInteger (maxBound :: Int)) roundedMicros))

    acquireTransport (host, port) = do
      let conn = connection state
      result <- connectTcp connectionApi conn host port
      case result of
        Left err -> pure (Left err)
        Right () -> pure (Right conn)

    handshakeFailure (HandshakeTransportError err) = ConnectTransportFailure err
    handshakeFailure (HandshakeTLSError err) = ConnectTLSFailure err
    handshakeFailure (HandshakeProtocolError err) = ConnectProtocolFailure err
    handshakeFailure (HandshakeAuthError err) = ConnectAuthenticationFailure (show err)

    failureCategory (ConnectTransportFailure _)      = "transport failure"
    failureCategory (ConnectTLSFailure _)            = "tls failure"
    failureCategory (ConnectProtocolFailure _)       = "protocol failure"
    failureCategory (ConnectAuthenticationFailure _) = "authentication failure"
    failureCategory ConnectHandshakeTimeout          = "handshake timeout"

    exitCategory ExitClosedByUser                  = "closed by user"
    exitCategory (ExitRetriesExhausted _)          = "retries exhausted"
    exitCategory (ExitServerError serverError)     = "server error: " ++ show (serverErrorKind serverError)
    exitCategory (ExitInboundMessageTooLarge _ _)  = "inbound message too large"
    exitCategory ExitResetRequested                = "reset requested"

    completeReadiness writeLock conn attemptEpoch =
      withSubscriptionGate state $
        (do
          result <- resubscribeIfNeeded writeLock conn
          case result of
            Left err -> do
              abort connectionApi conn
              pure (Left err)
            Right () -> do
              (accepted, rejectedPublishes) <-
                markConnectionReadyWithRejectedPublishes state attemptEpoch
              sequence_ rejectedPublishes
              unless accepted (abort connectionApi conn)
              pure (Right accepted))
        `onException` abort connectionApi conn

    resubscribeIfNeeded writeLock conn = do
      alreadyConnected <- connectedOnce state
      if alreadyConnected
        then do
          activeSubscriptions <- active store
          let resumable = filter isResumable activeSubscriptions
          result <- writeSubscriptions writeLock conn resumable
          let resumableCount = length resumable
          case result of
            Right () ->
              unless (resumableCount == 0) $
                runClient state
                  (logMessage Info ("resubscribed " ++ show resumableCount ++ " subscriptions"))
            Left _ -> pure ()
          pure result
        else pure (Right ())

    writeSubscriptions _ _ [] = pure (Right ())
    writeSubscriptions writeLock conn subscriptions = do
      let (chunk, remaining) = splitAt resubscribeChunkSize subscriptions
      result <- writeSerialized
        writeLock
        conn
        (transform (QueueBatch (map subscriptionCommand chunk)))
      case result of
        Left err -> pure (Left err)
        Right () -> writeSubscriptions writeLock conn remaining

    writeSerialized writeLock conn bytes =
      withMVar writeLock $ \_ ->
        writeDataLazy (writer connectionApi) conn bytes

    resubscribeChunkSize = 64

    subscriptionCommand (sid, meta@(SubscriptionMeta subject queueGroup _)) =
      QueueBatch $
        QueueItem
          Sub.Sub
            { Sub.subject = subject
            , Sub.queueGroup = queueGroup
            , Sub.sid = sid
            } :
          [ QueueItem
              Unsub.Unsub
                { Unsub.sid = sid
                , Unsub.maxMsg = Just 1
                }
          | isOneShotSubscription meta
          ]

    finalize reason = do
      closeStore store
      result <- markClosed state reason
      for_ result (exitAction (config state))

    startManaged :: IO () -> IO () -> IO ThreadId
    startManaged body cleanup =
      forkIOWithUnmask $ \unmask -> do
        threadId <- myThreadId
        registerManagedThread state threadId
        unmask body
          `finally` (cleanup `finally` unregisterManagedThread state threadId)

    startConnectionReader :: Conn -> MVar () -> IO ReaderRuntime
    startConnectionReader conn writeLock = do
      exitVar <- newEmptyTMVarIO
      readerDone <- newEmptyTMVarIO
      let stopReader = closeReader (reader connectionApi) conn
          stopWriter = closeWriter (writer connectionApi) conn
          stopQueue = closeQueue state
          signalReaderDone = atomically (void (tryPutTMVar readerDone ()))
          signalExit reason = atomically (void (tryPutTMVar exitVar reason))
          cleanup =
            ( do
                void (setReconnecting state)
                stopQueue
                stopWriter
                abort connectionApi conn
            ) `finally` signalReaderDone
          route message = do
            directive <- routeForConnection conn writeLock message
            case directive of
              RouteContinue -> pure ()
              RouteReconnect -> do
                signalExit ExitResetRequested
                stopQueue
                stopReader
                stopWriter
              RouteExit reason -> do
                setClosing state reason
                signalExit reason
                stopQueue
                stopReader
                stopWriter
      readerThread <- startManaged
        (runClient state $ do
          logMessage Debug "starting streaming thread"
          runStreaming
            (streamBufferLimit (messageLimit (config state)))
            (reader connectionApi)
            conn
            parserApi
            route
          logMessage Debug "streaming thread exited")
        cleanup
      pure
        ReaderRuntime
          { runtimeReaderThread = readerThread
          , runtimeReaderDone = readerDone
          , runtimeExit = exitVar
          }

    routeForConnection :: Conn -> MVar () -> ParsedMessage -> IO RouteDirective
    routeForConnection conn writeLock message@(ParsedPing _) = do
      status <- readStatus state
      case status of
        ConnectionConnected -> routeMessage state store message
        ConnectionClosing _  -> pure RouteReconnect
        ConnectionClosed _   -> pure RouteReconnect
        _ -> do
          runClient state (logMessage Debug "routing pre-ready PING")
          result <- writeSerialized writeLock conn (transform Pong)
          pure $ case result of
            Left _   -> RouteReconnect
            Right () -> RouteContinue
    routeForConnection _ _ message = routeMessage state store message

    terminateReader :: Conn -> ReaderRuntime -> IO ()
    terminateReader conn runtime = do
      closeReader (reader connectionApi) conn
      closeWriter (writer connectionApi) conn
      abort connectionApi conn
      killThread (runtimeReaderThread runtime)
      atomically (readTMVar (runtimeReaderDone runtime))

    runConnection :: Conn -> MVar () -> ReaderRuntime -> IO ConnectionRunResult
    runConnection conn writeLock readerRuntime = mask $ \restore -> do
      writerDone <- newEmptyTMVarIO

      let stopReader = closeReader (reader connectionApi) conn
          stopWriter = closeWriter (writer connectionApi) conn
          stopQueue = closeQueue state
          signalWriterDone = atomically (void (tryPutTMVar writerDone ()))

      writerThread <- startManaged
        (runClient state $ do
          logMessage Debug "starting broadcasting thread"
          runBroadcasting
            (queue state)
            (serializedWriter writeLock)
            conn
          logMessage Debug "broadcasting thread exited")
        ((stopQueue >> stopReader) `finally` signalWriterDone)

      let terminateChildren = do
            stopQueue
            stopReader
            stopWriter
            abort connectionApi conn
            killThread writerThread
            killThread (runtimeReaderThread readerRuntime)
            awaitChildren (runtimeReaderDone readerRuntime) writerDone

      outcome <- restore
        (atomically $
          (Left <$> readTMVar (runtimeExit readerRuntime))
            `orElse` (Right () <$ readTMVar (runtimeReaderDone readerRuntime))
            `orElse` (Right () <$ readTMVar writerDone))
        `onException` terminateChildren
      terminateChildren
      case outcome of
        Left ExitResetRequested -> pure ConnectionDisconnected
        Left reason             -> pure (ConnectionExit reason)
        Right ()                -> pure ConnectionDisconnected

      where
        awaitChildren readerDone writerDone = atomically $ do
          readTMVar readerDone
          readTMVar writerDone

    serializedWriter :: MVar () -> WriterAPI Conn
    serializedWriter writeLock =
      let baseWriter = writer connectionApi
      in WriterAPI
          { writeData = \conn bytes ->
              withMVar writeLock $ \_ -> writeData baseWriter conn bytes
          , writeDataLazy = \conn bytes ->
              withMVar writeLock $ \_ -> writeDataLazy baseWriter conn bytes
          , closeWriter = closeWriter baseWriter
          , openWriter = openWriter baseWriter
          }

    isResumable :: (a, SubscriptionMeta) -> Bool
    isResumable = isResumableSubscription . snd

    streamBufferLimit :: Int -> Int
    streamBufferLimit maximumMessageSize
      | maximumMessageSize > maxBound - controlLineAllowance = maxBound
      | otherwise = maximumMessageSize + controlLineAllowance

    controlLineAllowance = 64 * 1024

closeClient :: ConnectionAPI -> ClientState -> SubscriptionStore -> IO ()
closeClient connectionApi state store = mask_ $ do
  setClosing state ExitClosedByUser
  closeStore store
  closeQueue state
  closeReader (reader connectionApi) (connection state)
  closeWriter (writer connectionApi) (connection state)
  abort connectionApi (connection state)
  void . trySync . runClient state $
    logMessage Info "closing client connection"

resetClient :: ConnectionAPI -> ClientState -> SubscriptionStore -> IO ()
resetClient connectionApi state _store = mask $ \restore -> do
  invalidatedGeneration <- interruptConnection connectionApi state
  void . trySync . runClient state $
    logMessage Info "resetting client connection"
  current <- myThreadId
  managed <- isManagedThread state current
  unless managed . restore . atomically $
    waitForConnectionGenerationAfter state invalidatedGeneration
      `orElse` waitForClosed state

interruptConnection :: ConnectionAPI -> ClientState -> IO Int
interruptConnection connectionApi state = mask_ $ do
  invalidatedGeneration <- setReconnecting state
  closeReader (reader connectionApi) (connection state)
  closeWriter (writer connectionApi) (connection state)
  abort connectionApi (connection state)
  pure invalidatedGeneration
