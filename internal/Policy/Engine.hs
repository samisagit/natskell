module Engine
  ( closeClient
  , resetClient
  , runEngine
  ) where

import           Auth.Types
import           Control.Concurrent        (forkIO)
import           Control.Concurrent.STM
import           Control.Monad             (forM_, unless, void, when)
import           Data.Foldable             (for_)
import           Data.Maybe                (listToMaybe)
import           Handshake.Nats
    ( HandshakeError (..)
    , performHandshake
    )
import           Lib.Logger                (LogLevel (..), MonadLogger (..))
import           Network.ConnectionAPI
    ( Conn
    , ConnectionAPI
    , close
    , closeReader
    , closeWriter
    , connectTcp
    , open
    , reader
    , writer
    )
import           Parser.API                (ParsedMessage, ParserAPI)
import           Pipeline.Broadcasting.API (BroadcastingAPI (BroadcastingAPI))
import           Pipeline.Streaming.API    (StreamingAPI (StreamingAPI))
import           Queue.API                 (QueueItem (QueueItem))
import           Router.Nats               (RouteDirective (..), routeMessage)
import           State.Store
    ( ClientState
    , closeQueue
    , config
    , connection
    , enqueue
    , failInitialConnection
    , incrementAttemptIndex
    , markClosed
    , markConnected
    , markConnectionReady
    , openQueue
    , queue
    , readAttemptIndex
    , readConnectionGeneration
    , readStatus
    , runClient
    , setClosing
    , setEndpoint
    , waitForClosed
    , waitForConnectionGenerationAfter
    )
import           State.Types
    ( ClientConfig (..)
    , ClientExitReason (..)
    , ClientStatus (..)
    , ConnectAttemptError (..)
    , ConnectError (..)
    , ConnectFailure (..)
    )
import           Subscription.Store
    ( SubscriptionStore
    , active
    , awaitCallbackDrain
    , awaitNoTrackedExpiries
    )
import           Subscription.Types        (SubscriptionMeta (SubscriptionMeta))
import qualified Types.Sub                 as Sub

data ConnectionRunResult = ConnectionDisconnected
                         | ConnectionExit ClientExitReason

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
  case connectOptions (config state) of
    [] -> do
      failInitialConnection state ConnectNoServers
      finalize (ExitRetriesExhausted (Just "No servers provided"))
    _ ->
      loop (connectionAttempts (config state)) []
  where
    StreamingAPI runStreaming = streamingApi
    BroadcastingAPI runBroadcasting = broadcastingApi

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
            Running -> do
              attemptErr <- runAttempt
              nextStatus <- readStatus state
              case nextStatus of
                Running -> do
                  when (remaining > 1) $ do
                    runClient state $
                      logMessage Info "retrying client connection"
                    incrementAttemptIndex state
                  loop (remaining - 1) (attemptErr : attemptErrors)
                Closing reason ->
                  finalize reason
                Closed _ ->
                  pure ()
            Closing reason ->
              finalize reason
            Closed _ ->
              pure ()

    runAttempt = do
      openQueue state
      open connectionApi (connection state)
      let cfg = config state
          endpoints = connectOptions cfg
      attemptIndex <- readAttemptIndex state
      let endpoint@(host, _port) =
            endpoints !! (attemptIndex `mod` length endpoints)
      setEndpoint state endpoint
      transportResult <- acquireTransport endpoint
      case transportResult of
        Left err -> do
          runClient state $
            logMessage Error ("connection attempt failed: " ++ show err)
          close connectionApi (connection state)
          pure (ConnectAttemptError endpoint (ConnectTransportFailure err))
        Right conn -> do
          handshakeResult <- performHandshake connectionApi parserApi state auth conn host
          case handshakeResult of
            Left err -> do
              runClient state $
                logMessage Error ("connection initialization failed: " ++ show err)
              close connectionApi conn
              pure (ConnectAttemptError endpoint (handshakeFailure err))
            Right () -> do
              resubscribeIfNeeded
              markConnectionReady state
              connectionResult <- runConnection conn
              close connectionApi conn
              case connectionResult of
                ConnectionDisconnected -> do
                  runClient state $
                    logMessage Info "connection disconnected"
                  pure (ConnectAttemptError endpoint (ConnectTransportFailure "connection disconnected"))
                ConnectionExit reason -> do
                  setClosing state reason
                  pure (ConnectAttemptError endpoint (ConnectProtocolFailure (show reason)))

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
    handshakeFailure HandshakeTimeout = ConnectHandshakeTimeout

    resubscribeIfNeeded = do
      alreadyConnected <- markConnected state
      when alreadyConnected $ do
        activeSubscriptions <- active store
        let resumable = filter isResumable activeSubscriptions
        unless (null resumable) $
          runClient state
            (logMessage Info ("resubscribing " ++ show (length resumable) ++ " subscriptions"))
        forM_ resumable $ \(sid, SubscriptionMeta subject queueGroup _) ->
          enqueue state $
            QueueItem
              Sub.Sub
                { Sub.subject = subject
                , Sub.queueGroup = queueGroup
                , Sub.sid = sid
                }

    finalize reason = do
      result <- markClosed state reason
      for_ result (exitAction (config state))

    runConnection :: Conn -> IO ConnectionRunResult
    runConnection conn = do
      exitVar <- newEmptyTMVarIO
      readerDone <- newEmptyTMVarIO
      writerDone <- newEmptyTMVarIO

      let stopReader = closeReader (reader connectionApi) conn
          stopWriter = closeWriter (writer connectionApi) conn
          stopQueue = closeQueue state
          signalReaderDone = atomically (void (tryPutTMVar readerDone ()))
          signalWriterDone = atomically (void (tryPutTMVar writerDone ()))
          signalExit reason = atomically (void (tryPutTMVar exitVar reason))

      void . forkIO $ do
        runClient state $ do
          logMessage Debug "starting broadcasting thread"
          runBroadcasting
            (bufferLimit (config state))
            (queue state)
            (writer connectionApi)
            conn
          logMessage Debug "broadcasting thread exited"
        stopQueue
        stopReader
        signalWriterDone

      void . forkIO $ do
        runClient state $ do
          logMessage Debug "starting streaming thread"
          runStreaming
            (bufferLimit (config state))
            (reader connectionApi)
            conn
            parserApi
            (\message -> do
              directive <- routeMessage state store message
              case directive of
                RouteContinue ->
                  pure ()
                RouteExit reason -> do
                  setClosing state reason
                  signalExit reason
                  stopQueue
                  stopReader
                  stopWriter)
          logMessage Debug "streaming thread exited"
        stopQueue
        stopWriter
        signalReaderDone

      outcome <- atomically $
        (Left <$> readTMVar exitVar)
          `orElse` (Right () <$ readTMVar readerDone)
          `orElse` (Right () <$ readTMVar writerDone)
      atomically $ do
        readTMVar readerDone
        readTMVar writerDone
      case outcome of
        Left reason -> pure (ConnectionExit reason)
        Right ()    -> pure ConnectionDisconnected

    isResumable :: (a, SubscriptionMeta) -> Bool
    isResumable (_, SubscriptionMeta _ _ isReply) = not isReply

closeClient :: ConnectionAPI -> ClientState -> SubscriptionStore -> IO ()
closeClient connectionApi state store =
  shutdownClient connectionApi state store ExitClosedByUser "closing client connection"

resetClient :: ConnectionAPI -> ClientState -> SubscriptionStore -> IO ()
resetClient connectionApi state _store = do
  generation <- readConnectionGeneration state
  runClient state $
    logMessage Info "resetting client connection"
  closeQueue state
  closeReader (reader connectionApi) (connection state)
  closeWriter (writer connectionApi) (connection state)
  atomically $
    waitForConnectionGenerationAfter state generation
      `orElse` waitForClosed state

shutdownClient
  :: ConnectionAPI
  -> ClientState
  -> SubscriptionStore
  -> ClientExitReason
  -> String
  -> IO ()
shutdownClient connectionApi state store reason message = do
  setClosing state reason
  runClient state $
    logMessage Info message
  closeQueue state
  closeReader (reader connectionApi) (connection state)
  closeWriter (writer connectionApi) (connection state)
  atomically $ waitForClosed state
  atomically $ awaitNoTrackedExpiries store
  atomically $ awaitCallbackDrain store
