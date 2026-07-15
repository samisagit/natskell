{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module State.Store
  ( ClientState
  , newClientState
  , runClient
  , config
  , queue
  , connection
  , enqueue
  , tryEnqueue
  , openQueue
  , closeQueue
  , discardConnectionScoped
  , PingResult (..)
  , registerPingWaiter
  , registerPingWaiterAndEnqueue
  , resolveNextPing
  , clearPendingPings
  , nextSid
  , nextInbox
  , readServerInfo
  , setServerInfo
  , updateLogContextFromInfo
  , setConnectName
  , setEndpoint
  , ServerErrorEnqueueResult (..)
  , notifyServerError
  , CallbackWorker
  , startCallbackWorker
  , callbackWorkerThreadId
  , waitForCallbackWorker
  , stopCallbackWorker
  , readStatus
  , waitForConnected
  , beginConnectionAttempt
  , activateConnectionAttempt
  , setReconnecting
  , setClosing
  , markClosed
  , waitForClosed
  , waitForNotRunning
  , waitForServerInfo
  , connectedOnce
  , markConnectionReady
  , markConnectionReadyWithRejectedPublishes
  , waitForInitialConnection
  , failInitialConnection
  , readConnectionGeneration
  , waitForConnectionGenerationAfter
  , waitForConnectionGenerationLoss
  , enqueueOnGenerationTracked
  , PublishEnqueueResult (..)
  , enqueuePublishOnConnected
  , enqueuePublishOnConnectedGenerationTracked
  , tryEnqueueOnGeneration
  , readAttemptIndex
  , incrementAttemptIndex
  , registerManagedThread
  , unregisterManagedThread
  , isManagedThread
  , withSubscriptionGate
  ) where

import           Control.Concurrent
    ( ThreadId
    , forkIOWithUnmask
    , killThread
    , myThreadId
    )
import           Control.Concurrent.STM
import           Control.Exception      (bracket_, finally)
import           Control.Monad          (unless, void, when)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BC
import           Data.List              (delete)
import           Lib.Exception          (trySync)
import           Lib.Logger
    ( LogLevel (..)
    , MonadLogger (..)
    , loggerApi
    )
import           Lib.Logger.Types       (AppM, LogContext (..))
import           Lib.LoggerAPI          (runWithLogger, updateLogContext)
import           Network.ConnectionAPI  (Conn)
import           Nuid                   (Nuid, newNuidIO, nextNuid)
import qualified Queue.API              as Queue
import           Queue.API              (Queue, QueueItem, TryEnqueueResult)
import           Sid                    (SIDCounter, initialSIDCounter, nextSID)
import           State.Types
import qualified Types.Info             as Info
import           Types.Info             (Info, client_id)
import           Types.Msg              (SID, Subject)

data ClientState = ClientState
                     { clientConfig :: ClientConfig
                     , clientQueue :: Queue
                     , clientPings :: TQueue (Int, TMVar PingResult)
                     , clientCallbacks :: TQueue ClientCallback
                     , clientPendingServerCallbacks :: TVar Int
                     , clientPendingServerCallbackBytes :: TVar Int
                     , clientServerCallbackOverflowed :: TVar Bool
                     , clientConnectedOnce :: TVar Bool
                     , clientConnectionGeneration :: TVar Int
                     , clientInitialConnection :: TMVar (Either ConnectError ())
                     , clientSidCounter :: TVar SIDCounter
                     , clientInboxNuid :: TVar Nuid
                     , clientServerInfo :: TVar (Maybe Info)
                     , clientAttemptIndex :: TVar Int
                     , clientAttemptEpoch :: TVar Int
                     , clientStatus :: TVar ConnectionState
                     , clientConnection :: Conn
                     , clientLogContext :: TVar LogContext
                     , clientManagedThreads :: TVar [ThreadId]
                     , clientSubscriptionGate :: TMVar ()
                     }

data PingResult = PingReceived | PingConnectionLost
  deriving (Eq, Show)

data PublishEnqueueResult = PublishEnqueued Int
                          | PublishTooLarge Int
                          | PublishConnectionClosed ClientExitReason
  deriving (Eq, Show)

data ClientCallback = CallbackServerError ServerError
                    | CallbackConnectionEvent ConnectionEvent

data ServerErrorEnqueueResult = ServerErrorQueued | ServerErrorDropped | ServerErrorDroppedReport
  deriving (Eq, Show)

data CallbackWorker = CallbackWorker
                        { callbackWorkerThreadId :: ThreadId
                        , callbackWorkerDone     :: TMVar ()
                        }

newClientState :: ClientConfig -> Queue -> Conn -> TVar LogContext -> IO ClientState
newClientState cfg queue conn logContext = do
  Queue.close queue
  pings <- newTQueueIO
  callbacks <- newTQueueIO
  pendingServerCallbacks <- newTVarIO 0
  pendingServerCallbackBytes <- newTVarIO 0
  serverCallbackOverflowed <- newTVarIO False
  connectedOnce <- newTVarIO False
  connectionGeneration <- newTVarIO 0
  initialConnection <- newEmptyTMVarIO
  sidCounter <- newTVarIO initialSIDCounter
  inboxNuid <- newTVarIO =<< newNuidIO
  serverInfo <- newTVarIO Nothing
  attemptIndex <- newTVarIO 0
  attemptEpoch <- newTVarIO 0
  status <- newTVarIO ConnectionConnecting
  managedThreads <- newTVarIO []
  subscriptionGate <- newTMVarIO ()
  pure ClientState
    { clientConfig = cfg
    , clientQueue = queue
    , clientPings = pings
    , clientCallbacks = callbacks
    , clientPendingServerCallbacks = pendingServerCallbacks
    , clientPendingServerCallbackBytes = pendingServerCallbackBytes
    , clientServerCallbackOverflowed = serverCallbackOverflowed
    , clientConnectedOnce = connectedOnce
    , clientConnectionGeneration = connectionGeneration
    , clientInitialConnection = initialConnection
    , clientSidCounter = sidCounter
    , clientInboxNuid = inboxNuid
    , clientServerInfo = serverInfo
    , clientAttemptIndex = attemptIndex
    , clientAttemptEpoch = attemptEpoch
    , clientStatus = status
    , clientConnection = conn
    , clientLogContext = logContext
    , clientManagedThreads = managedThreads
    , clientSubscriptionGate = subscriptionGate
    }

runClient :: ClientState -> AppM a -> IO a
runClient client =
  runWithLogger loggerApi (loggerConfig (clientConfig client)) (clientLogContext client)

config :: ClientState -> ClientConfig
config = clientConfig

queue :: ClientState -> Queue
queue = clientQueue

connection :: ClientState -> Conn
connection = clientConnection

enqueue :: ClientState -> QueueItem -> IO (Either String ())
enqueue client = Queue.enqueue (clientQueue client)

tryEnqueue :: ClientState -> QueueItem -> IO TryEnqueueResult
tryEnqueue client = Queue.tryEnqueue (clientQueue client)

openQueue :: ClientState -> IO ()
openQueue = Queue.open . clientQueue

closeQueue :: ClientState -> IO ()
closeQueue = Queue.close . clientQueue

discardConnectionScoped :: ClientState -> IO ()
discardConnectionScoped = Queue.discardConnectionScoped . clientQueue

registerPingWaiter :: ClientState -> TMVar PingResult -> IO (Either ClientExitReason ())
registerPingWaiter client waiter = atomically $ do
  status <- readTVar (clientStatus client)
  case status of
    ConnectionConnecting   -> retry
    ConnectionReconnecting -> retry
    ConnectionConnected -> do
      generation <- readTVar (clientConnectionGeneration client)
      writeTQueue (clientPings client) (generation, waiter)
      pure (Right ())
    ConnectionClosing reason -> pure (Left reason)
    ConnectionClosed reason  -> pure (Left reason)

registerPingWaiterAndEnqueue
  :: ClientState
  -> TMVar PingResult
  -> QueueItem
  -> IO (Either ClientExitReason Int)
registerPingWaiterAndEnqueue client waiter item = do
  connected <- atomically (waitForConnectedGeneration client)
  case connected of
    Left reason -> pure (Left reason)
    Right generation -> atomically $ do
      result <- enqueueOnGenerationSTM client generation item
      case result of
        Left reason -> pure (Left reason)
        Right () -> do
          writeTQueue (clientPings client) (generation, waiter)
          pure (Right generation)

enqueueOnGenerationTracked
  :: ClientState
  -> Int
  -> TMVar Int
  -> QueueItem
  -> IO (Either ClientExitReason Int)
enqueueOnGenerationTracked client expectedGeneration committedGeneration item =
  atomically $ do
    result <- enqueueOnGenerationSTM client expectedGeneration item
    case result of
      Left reason -> pure (Left reason)
      Right () -> do
        putTMVar committedGeneration expectedGeneration
        pure (Right expectedGeneration)

enqueuePublishOnConnected
  :: ClientState
  -> Int
  -> QueueItem
  -> IO PublishEnqueueResult
enqueuePublishOnConnected client actualSize item =
  atomically (enqueuePublishOnConnectedSTM client Nothing actualSize item)

enqueuePublishOnConnectedGenerationTracked
  :: ClientState
  -> TMVar Int
  -> Int
  -> QueueItem
  -> IO PublishEnqueueResult
enqueuePublishOnConnectedGenerationTracked client committedGeneration actualSize item =
  atomically $
    enqueuePublishOnConnectedSTM client (Just committedGeneration) actualSize item

enqueuePublishOnConnectedSTM
  :: ClientState
  -> Maybe (TMVar Int)
  -> Int
  -> QueueItem
  -> STM PublishEnqueueResult
enqueuePublishOnConnectedSTM client committedGeneration actualSize item = do
  status <- readTVar (clientStatus client)
  case status of
    ConnectionConnecting   -> retry
    ConnectionReconnecting -> retry
    ConnectionConnected -> do
      generation <- readTVar (clientConnectionGeneration client)
      serverInfo <- readTVar (clientServerInfo client)
      let maximumSize = effectivePayloadLimit client serverInfo
      if actualSize > maximumSize
        then pure (PublishTooLarge maximumSize)
        else do
          queued <- Queue.enqueueSTM (clientQueue client) item
          case queued of
            Left _ -> pure (PublishConnectionClosed ExitResetRequested)
            Right () -> do
              mapM_ (`putTMVar` generation) committedGeneration
              pure (PublishEnqueued generation)
    ConnectionClosing reason -> pure (PublishConnectionClosed reason)
    ConnectionClosed reason  -> pure (PublishConnectionClosed reason)

effectivePayloadLimit :: ClientState -> Maybe Info -> Int
effectivePayloadLimit client =
  maybe
    (messageLimit (clientConfig client))
    (min (messageLimit (clientConfig client)) . Info.max_payload)

waitForConnectedGeneration :: ClientState -> STM (Either ClientExitReason Int)
waitForConnectedGeneration client = do
  status <- readTVar (clientStatus client)
  case status of
    ConnectionConnecting   -> retry
    ConnectionReconnecting -> retry
    ConnectionConnected ->
      Right <$> readTVar (clientConnectionGeneration client)
    ConnectionClosing reason -> pure (Left reason)
    ConnectionClosed reason  -> pure (Left reason)

enqueueOnGenerationSTM
  :: ClientState
  -> Int
  -> QueueItem
  -> STM (Either ClientExitReason ())
enqueueOnGenerationSTM client expectedGeneration item = do
  status <- readTVar (clientStatus client)
  generation <- readTVar (clientConnectionGeneration client)
  case status of
    ConnectionConnecting   -> pure (Left ExitResetRequested)
    ConnectionReconnecting -> pure (Left ExitResetRequested)
    ConnectionConnected -> do
      if generation /= expectedGeneration
        then pure (Left ExitResetRequested)
        else Queue.enqueueSTM (clientQueue client) item >>= \case
          Right () -> pure (Right ())
          Left _   -> pure (Left ExitResetRequested)
    ConnectionClosing reason -> pure (Left reason)
    ConnectionClosed reason  -> pure (Left reason)

tryEnqueueOnGeneration
  :: ClientState
  -> Int
  -> QueueItem
  -> IO TryEnqueueResult
tryEnqueueOnGeneration client expectedGeneration item = atomically $ do
  status <- readTVar (clientStatus client)
  generation <- readTVar (clientConnectionGeneration client)
  if status == ConnectionConnected && generation == expectedGeneration
    then Queue.tryEnqueueSTM (clientQueue client) item
    else pure Queue.TryQueueClosed

resolveNextPing :: ClientState -> IO ()
resolveNextPing client =
  atomically $ do
    generation <- readTVar (clientConnectionGeneration client)
    resolveNextPingSTM client generation

resolveNextPingSTM :: ClientState -> Int -> STM ()
resolveNextPingSTM client expectedGeneration = do
  currentGeneration <- readTVar (clientConnectionGeneration client)
  when (expectedGeneration == currentGeneration) resolveCurrent
  where
    resolveCurrent = do
      pending <- tryReadTQueue (clientPings client)
      case pending of
        Nothing -> pure ()
        Just (generation, waiter) -> do
          when (generation == expectedGeneration) $
            void (tryPutTMVar waiter PingReceived)

clearPendingPings :: ClientState -> IO ()
clearPendingPings = atomically . clearPendingPingsSTM

nextSid :: ClientState -> IO SID
nextSid client =
  atomically $ do
    counter <- readTVar (clientSidCounter client)
    let (sid, counter') = nextSID counter
    writeTVar (clientSidCounter client) counter'
    pure sid

nextInbox :: ClientState -> IO Subject
nextInbox client =
  atomically $ do
    nuid <- readTVar (clientInboxNuid client)
    let (token, nextNuidValue) = nextNuid nuid
    writeTVar (clientInboxNuid client) nextNuidValue
    pure ("_INBOX." <> token)

readServerInfo :: ClientState -> IO (Maybe Info)
readServerInfo = readTVarIO . clientServerInfo

setServerInfo :: ClientState -> Info -> IO ()
setServerInfo client info =
  atomically $ writeTVar (clientServerInfo client) (Just info)

updateLogContextFromInfo :: ClientState -> Info -> IO ()
updateLogContextFromInfo client info =
  updateLogContext loggerApi (clientLogContext client) $ \ctx ->
    ctx { lcClientId = client_id info }

setConnectName :: ClientState -> Maybe BS.ByteString -> IO ()
setConnectName client maybeName =
  updateLogContext loggerApi (clientLogContext client) $ \ctx ->
    ctx { lcConnectName = BC.unpack <$> maybeName }

setEndpoint :: ClientState -> (String, Int) -> IO ()
setEndpoint client (host, port) =
  updateLogContext loggerApi (clientLogContext client) $ \ctx ->
    ctx { lcServer = Just (host ++ ":" ++ show port) }

notifyServerError :: ClientState -> ServerError -> IO ServerErrorEnqueueResult
notifyServerError client = atomically . enqueueServerErrorSTM client

enqueueServerErrorSTM :: ClientState -> ServerError -> STM ServerErrorEnqueueResult
enqueueServerErrorSTM client serverError = do
  pending <- readTVar (clientPendingServerCallbacks client)
  pendingBytes <- readTVar (clientPendingServerCallbackBytes client)
  let reasonBytes = BS.length (serverErrorReason serverError)
      hasCapacity =
        pending < serverCallbackLimit
          && reasonBytes <= serverCallbackByteLimit - pendingBytes
  if hasCapacity
    then do
      modifyTVar' (clientPendingServerCallbacks client) (+ 1)
      modifyTVar' (clientPendingServerCallbackBytes client) (+ reasonBytes)
      writeTQueue (clientCallbacks client) (CallbackServerError serverError)
      pure ServerErrorQueued
    else do
      alreadyReported <- readTVar (clientServerCallbackOverflowed client)
      if alreadyReported
        then pure ServerErrorDropped
        else do
          writeTVar (clientServerCallbackOverflowed client) True
          pure ServerErrorDroppedReport

enqueueCallbackSTM :: ClientState -> ClientCallback -> STM Bool
enqueueCallbackSTM client event =
  case event of
    CallbackServerError serverError ->
      (== ServerErrorQueued) <$> enqueueServerErrorSTM client serverError
    CallbackConnectionEvent _ -> do
      writeTQueue (clientCallbacks client) event
      pure True

serverCallbackLimit :: Int
serverCallbackLimit = 256

serverCallbackByteLimit :: Int
serverCallbackByteLimit = 1024 * 1024

startCallbackWorker :: ClientState -> IO CallbackWorker
startCallbackWorker client = do
  done <- newEmptyTMVarIO
  thread <- forkIOWithUnmask $ \unmask -> do
    threadId <- myThreadId
    let signalDone = atomically (void (tryPutTMVar done ()))
    registerManagedThread client threadId
    (unmask loop `finally` unregisterManagedThread client threadId)
      `finally` signalDone
  pure (CallbackWorker thread done)
  where
    loop = do
      next <- atomically $
        (Just <$> readTQueue (clientCallbacks client))
          `orElse` do
            status <- readTVar (clientStatus client)
            case status of
              ConnectionClosed _ -> do
                empty <- isEmptyTQueue (clientCallbacks client)
                check empty
                pure Nothing
              _ -> retry
      case next of
        Nothing -> pure ()
        Just (CallbackServerError serverError) -> do
          atomically $ do
            modifyTVar' (clientPendingServerCallbacks client) (subtract 1)
            modifyTVar'
              (clientPendingServerCallbackBytes client)
              (subtract (BS.length (serverErrorReason serverError)))
            pending <- readTVar (clientPendingServerCallbacks client)
            pendingBytes <- readTVar (clientPendingServerCallbackBytes client)
            when
              ( pending <= serverCallbackLimit `div` 2
                  && pendingBytes <= serverCallbackByteLimit `div` 2
              )
              (writeTVar (clientServerCallbackOverflowed client) False)
          result <- trySync (serverErrorHandler (clientConfig client) serverError)
          case result of
            Right () -> pure ()
            Left _ ->
              runClient client $
                logMessage Error "server error handler failed"
          loop
        Just (CallbackConnectionEvent event) -> do
          result <- trySync (connectionEventHandler (clientConfig client) event)
          case result of
            Right () -> pure ()
            Left _ ->
              runClient client $
                logMessage Error "connection event handler failed"
          loop

waitForCallbackWorker :: CallbackWorker -> STM ()
waitForCallbackWorker = readTMVar . callbackWorkerDone

stopCallbackWorker :: CallbackWorker -> IO ()
stopCallbackWorker worker = do
  killThread (callbackWorkerThreadId worker)
  atomically (waitForCallbackWorker worker)

readStatus :: ClientState -> IO ConnectionState
readStatus = readTVarIO . clientStatus

waitForConnected :: ClientState -> STM (Either ClientExitReason ())
waitForConnected client = do
  status <- readTVar (clientStatus client)
  case status of
    ConnectionConnecting     -> retry
    ConnectionConnected      -> pure (Right ())
    ConnectionReconnecting   -> retry
    ConnectionClosing reason -> pure (Left reason)
    ConnectionClosed reason  -> pure (Left reason)

setReconnecting :: ClientState -> IO Int
setReconnecting client =
  atomically $ do
    generation <- readTVar (clientConnectionGeneration client)
    previous <- readTVar (clientStatus client)
    case previous of
      ConnectionClosing _ -> pure ()
      ConnectionClosed _  -> pure ()
      ConnectionConnected -> do
        writeTVar (clientStatus client) ConnectionReconnecting
        void $ enqueueCallbackSTM client
          (CallbackConnectionEvent ConnectionEventDisconnected)
      _ -> writeTVar (clientStatus client) ConnectionReconnecting
    modifyTVar' (clientAttemptEpoch client) (+ 1)
    clearPendingPingsSTM client
    Queue.closeAndDiscardConnectionScoped (clientQueue client)
    pure generation

setClosing :: ClientState -> ClientExitReason -> IO ()
setClosing client reason =
  atomically $ do
    modifyTVar' (clientStatus client) $ \case
      ConnectionClosed result  -> ConnectionClosed result
      ConnectionClosing result -> ConnectionClosing result
      ConnectionConnecting     -> ConnectionClosing reason
      ConnectionConnected      -> ConnectionClosing reason
      ConnectionReconnecting   -> ConnectionClosing reason
    modifyTVar' (clientAttemptEpoch client) (+ 1)
    clearPendingPingsSTM client
    Queue.closeAndDiscardAll (clientQueue client)

beginConnectionAttempt :: ClientState -> IO (Maybe Int)
beginConnectionAttempt client = atomically $ do
  status <- readTVar (clientStatus client)
  case status of
    ConnectionConnecting   -> begin
    ConnectionReconnecting -> begin
    ConnectionConnected    -> pure Nothing
    ConnectionClosing _    -> pure Nothing
    ConnectionClosed _     -> pure Nothing
  where
    begin = do
      modifyTVar' (clientAttemptEpoch client) (+ 1)
      epoch <- readTVar (clientAttemptEpoch client)
      pure (Just epoch)

activateConnectionAttempt :: ClientState -> Int -> IO Bool
activateConnectionAttempt client attemptEpoch = atomically $ do
  status <- readTVar (clientStatus client)
  currentEpoch <- readTVar (clientAttemptEpoch client)
  if currentEpoch /= attemptEpoch
    then pure False
    else case status of
      ConnectionConnecting   -> activate
      ConnectionReconnecting -> activate
      ConnectionConnected    -> pure False
      ConnectionClosing _    -> pure False
      ConnectionClosed _     -> pure False
  where
    activate = pure True

markClosed :: ClientState -> ClientExitReason -> IO (Maybe ClientExitReason)
markClosed client fallbackReason =
  atomically $ do
    clearPendingPingsSTM client
    Queue.closeAndDiscardAll (clientQueue client)
    status <- readTVar (clientStatus client)
    case status of
      ConnectionClosed _ ->
        pure Nothing
      ConnectionClosing reason -> do
        writeTVar (clientStatus client) (ConnectionClosed reason)
        void $ enqueueCallbackSTM client
          (CallbackConnectionEvent (ConnectionEventClosed reason))
        pure (Just reason)
      _ -> do
        writeTVar (clientStatus client) (ConnectionClosed fallbackReason)
        void $ enqueueCallbackSTM client
          (CallbackConnectionEvent (ConnectionEventClosed fallbackReason))
        pure (Just fallbackReason)

waitForClosed :: ClientState -> STM ()
waitForClosed client = do
  status <- readTVar (clientStatus client)
  case status of
    ConnectionClosed _ -> pure ()
    _                  -> retry

waitForNotRunning :: ClientState -> STM ()
waitForNotRunning client = do
  status <- readTVar (clientStatus client)
  case status of
    ConnectionClosing _ -> pure ()
    ConnectionClosed _  -> pure ()
    _                   -> retry

waitForServerInfo :: ClientState -> STM ()
waitForServerInfo client = do
  info <- readTVar (clientServerInfo client)
  case info of
    Just _  -> pure ()
    Nothing -> retry

connectedOnce :: ClientState -> IO Bool
connectedOnce = readTVarIO . clientConnectedOnce

markConnectionReady :: ClientState -> Int -> IO Bool
markConnectionReady client attemptEpoch = do
  (accepted, rejectedPublishes) <-
    markConnectionReadyWithRejectedPublishes client attemptEpoch
  sequence_ rejectedPublishes
  pure accepted

markConnectionReadyWithRejectedPublishes
  :: ClientState
  -> Int
  -> IO (Bool, [IO ()])
markConnectionReadyWithRejectedPublishes client attemptEpoch =
  atomically $ do
    status <- readTVar (clientStatus client)
    currentEpoch <- readTVar (clientAttemptEpoch client)
    if currentEpoch /= attemptEpoch
      then pure (False, [])
      else case status of
        ConnectionConnecting   -> markReady
        ConnectionReconnecting -> markReady
        ConnectionConnected    -> pure (False, [])
        ConnectionClosing _    -> pure (False, [])
        ConnectionClosed _     -> pure (False, [])
  where
    markReady = do
      wasConnected <- readTVar (clientConnectedOnce client)
      serverInfo <- readTVar (clientServerInfo client)
      rejectedPublishes <-
        Queue.discardOversizedPayloads
          (clientQueue client)
          (effectivePayloadLimit client serverInfo)
      modifyTVar' (clientConnectionGeneration client) (+ 1)
      writeTVar (clientConnectedOnce client) True
      writeTVar (clientStatus client) ConnectionConnected
      Queue.openSTM (clientQueue client)
      void (tryPutTMVar (clientInitialConnection client) (Right ()))
      when wasConnected . void $ enqueueCallbackSTM client
        (CallbackConnectionEvent ConnectionEventReconnected)
      pure (True, rejectedPublishes)

waitForInitialConnection :: ClientState -> STM (Either ConnectError ())
waitForInitialConnection = readTMVar . clientInitialConnection

failInitialConnection :: ClientState -> ConnectError -> IO ()
failInitialConnection client err =
  atomically $ void (tryPutTMVar (clientInitialConnection client) (Left err))

readConnectionGeneration :: ClientState -> IO Int
readConnectionGeneration =
  readTVarIO . clientConnectionGeneration

waitForConnectionGenerationAfter :: ClientState -> Int -> STM ()
waitForConnectionGenerationAfter client generation = do
  status <- readTVar (clientStatus client)
  current <- readTVar (clientConnectionGeneration client)
  unless (status == ConnectionConnected && current > generation) retry

waitForConnectionGenerationLoss :: ClientState -> Int -> STM ClientExitReason
waitForConnectionGenerationLoss client expectedGeneration = do
  status <- readTVar (clientStatus client)
  generation <- readTVar (clientConnectionGeneration client)
  case status of
    ConnectionConnected
      | generation == expectedGeneration -> retry
      | otherwise -> pure ExitResetRequested
    ConnectionClosing reason -> pure reason
    ConnectionClosed reason  -> pure reason
    ConnectionConnecting     -> pure ExitResetRequested
    ConnectionReconnecting   -> pure ExitResetRequested

readAttemptIndex :: ClientState -> IO Int
readAttemptIndex = readTVarIO . clientAttemptIndex

incrementAttemptIndex :: ClientState -> IO ()
incrementAttemptIndex client =
  atomically (modifyTVar' (clientAttemptIndex client) (+ 1))

registerManagedThread :: ClientState -> ThreadId -> IO ()
registerManagedThread client threadId =
  atomically $ modifyTVar' (clientManagedThreads client) (threadId :)

unregisterManagedThread :: ClientState -> ThreadId -> IO ()
unregisterManagedThread client threadId =
  atomically $ modifyTVar' (clientManagedThreads client) (delete threadId)

isManagedThread :: ClientState -> ThreadId -> IO Bool
isManagedThread client threadId =
  elem threadId <$> readTVarIO (clientManagedThreads client)

withSubscriptionGate :: ClientState -> IO a -> IO a
withSubscriptionGate client =
  bracket_
    (atomically (takeTMVar (clientSubscriptionGate client)))
    (atomically (putTMVar (clientSubscriptionGate client) ()))

clearPendingPingsSTM :: ClientState -> STM ()
clearPendingPingsSTM client = loop
  where
    loop = do
      waiter <- tryReadTQueue (clientPings client)
      case waiter of
        Nothing -> pure ()
        Just (_, result) -> do
          void (tryPutTMVar result PingConnectionLost)
          loop
