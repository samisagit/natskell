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
  , openQueue
  , closeQueue
  , pushPingAction
  , runNextPingAction
  , nextSid
  , nextInbox
  , readServerInfo
  , setServerInfo
  , updateLogContextFromInfo
  , setConnectName
  , setEndpoint
  , readStatus
  , setClosing
  , markClosed
  , waitForClosed
  , waitForNotRunning
  , waitForServerInfo
  , markConnected
  , readAttemptIndex
  , incrementAttemptIndex
  ) where

import           Control.Concurrent.STM
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BC
import           Data.Maybe             (fromMaybe)
import           Lib.Logger
    ( LogLevel (..)
    , MonadLogger (..)
    , loggerApi
    )
import           Lib.Logger.Types       (AppM, LogContext (..))
import           Lib.LoggerAPI          (runWithLogger, updateLogContext)
import           Network.ConnectionAPI  (Conn)
import           Nuid                   (Nuid, newNuidIO, nextNuid)
import           Queue.API              (Queue (Queue), QueueItem, close, open)
import           Sid                    (SIDCounter, initialSIDCounter, nextSID)
import           State.Types
import           Types.Info             (Info, client_id)
import           Types.Msg              (SID, Subject)

data ClientState = ClientState
                     { clientConfig        :: ClientConfig
                     , clientQueue         :: Queue
                     , clientPings         :: TQueue (IO ())
                     , clientConnectedOnce :: TVar Bool
                     , clientSidCounter    :: TVar SIDCounter
                     , clientInboxNuid     :: TVar Nuid
                     , clientServerInfo    :: TVar (Maybe Info)
                     , clientAttemptIndex  :: TVar Int
                     , clientStatus        :: TVar ClientStatus
                     , clientConnection    :: Conn
                     , clientLogContext    :: TVar LogContext
                     }

newClientState :: ClientConfig -> Queue -> Conn -> TVar LogContext -> IO ClientState
newClientState cfg queue conn logContext = do
  pings <- newTQueueIO
  connectedOnce <- newTVarIO False
  sidCounter <- newTVarIO initialSIDCounter
  inboxNuid <- newTVarIO =<< newNuidIO
  serverInfo <- newTVarIO Nothing
  attemptIndex <- newTVarIO 0
  status <- newTVarIO Running
  pure ClientState
    { clientConfig = cfg
    , clientQueue = queue
    , clientPings = pings
    , clientConnectedOnce = connectedOnce
    , clientSidCounter = sidCounter
    , clientInboxNuid = inboxNuid
    , clientServerInfo = serverInfo
    , clientAttemptIndex = attemptIndex
    , clientStatus = status
    , clientConnection = conn
    , clientLogContext = logContext
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

enqueue :: ClientState -> QueueItem -> IO ()
enqueue client item = do
  let Queue queueEnqueue _ _ _ = clientQueue client
  result <- queueEnqueue item
  case result of
    Left err ->
      runClient client $
        logMessage Error ("enqueueing item failed: " ++ err)
    Right () ->
      pure ()

openQueue :: ClientState -> IO ()
openQueue = open . clientQueue

closeQueue :: ClientState -> IO ()
closeQueue = close . clientQueue

pushPingAction :: ClientState -> IO () -> IO ()
pushPingAction client action =
  atomically $ writeTQueue (clientPings client) action

runNextPingAction :: ClientState -> IO ()
runNextPingAction client = do
  action <- atomically $ tryReadTQueue (clientPings client)
  fromMaybe (pure ()) action

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

readStatus :: ClientState -> IO ClientStatus
readStatus = readTVarIO . clientStatus

setClosing :: ClientState -> ClientExitReason -> IO ()
setClosing client reason =
  atomically . modifyTVar' (clientStatus client) $ \case
    Closed result  -> Closed result
    Closing result -> Closing result
    Running        -> Closing reason

markClosed :: ClientState -> ClientExitReason -> IO (Maybe ClientExitReason)
markClosed client fallbackReason =
  atomically $ do
    status <- readTVar (clientStatus client)
    case status of
      Closed _ ->
        pure Nothing
      Closing reason -> do
        writeTVar (clientStatus client) (Closed reason)
        pure (Just reason)
      Running -> do
        writeTVar (clientStatus client) (Closed fallbackReason)
        pure (Just fallbackReason)

waitForClosed :: ClientState -> STM ()
waitForClosed client = do
  status <- readTVar (clientStatus client)
  case status of
    Closed _ -> pure ()
    _        -> retry

waitForNotRunning :: ClientState -> STM ()
waitForNotRunning client = do
  status <- readTVar (clientStatus client)
  case status of
    Running -> retry
    _       -> pure ()

waitForServerInfo :: ClientState -> STM ()
waitForServerInfo client = do
  info <- readTVar (clientServerInfo client)
  case info of
    Just _  -> pure ()
    Nothing -> retry

markConnected :: ClientState -> IO Bool
markConnected client =
  atomically $ do
    alreadyConnected <- readTVar (clientConnectedOnce client)
    writeTVar (clientConnectedOnce client) True
    pure alreadyConnected

readAttemptIndex :: ClientState -> IO Int
readAttemptIndex = readTVarIO . clientAttemptIndex

incrementAttemptIndex :: ClientState -> IO ()
incrementAttemptIndex client =
  atomically (modifyTVar' (clientAttemptIndex client) (+ 1))
