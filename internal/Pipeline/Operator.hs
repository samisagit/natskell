module Pipeline.Operator (newConnection, updateStatus, Logger(..), withLogger, withConnectPairs) where

import           Control.Concurrent.STM
import           GHC.IO.Handle

data Status = Connecting | Connected | Disconnecting | Disconnected | Draining
  deriving (Eq, Show)

data Event = UnexpectedDisconnect String
           | ConnectedEvent
           | FatalErrorEvent String
           | UserCloseEvent
  deriving (Eq, Show)

type ConnectPair = (String, Int)

data Connection = Connection
                    { status       :: Status
                    , socket       :: Handle
                    , connectPairs :: Maybe [ConnectPair]
                    , connectFunc  :: ConnectPair -> IO Handle
                    , logger       :: Maybe Logger
                    }

-- this should probably be in a separate module as it will be used for things other than pipeline
data Logger = Logger
                { logInfo  :: String -> IO ()
                , logError :: String -> IO ()
                , logDebug :: String -> IO ()
                , logFatal :: String -> IO ()
                }

type ConnectionOpts = (Connection -> Connection)

withLogger :: Logger -> ConnectionOpts
withLogger logger conn = conn { logger = Just logger }

-- perhaps set up the pairs to cycle when used/retried
withConnectPairs :: [ConnectPair] -> ConnectionOpts
withConnectPairs pairs conn = conn { connectPairs = Just pairs }

newConnection :: ConnectPair -> (ConnectPair -> IO Handle) -> [ConnectionOpts] -> IO (TVar Connection)
newConnection pairs func opts = do
  handle <- func pairs
  let conn = Connection {
    status = Connecting
    , socket = handle
    , connectFunc = func
    , connectPairs = Nothing
    , logger = Nothing
  }
  let conn' = foldr ($) conn opts
  newTVarIO conn'

updateStatus :: TVar Connection -> Event -> STM ()
updateStatus connVar ConnectedEvent = modifyTVar' connVar $ \c -> c { status = Connected }
updateStatus connVar (FatalErrorEvent reason) = modifyTVar' connVar $ \c -> c { status = Disconnecting }
updateStatus connVar (UnexpectedDisconnect error) = modifyTVar' connVar $ \c -> c { status = Disconnected }
updateStatus connVar UserCloseEvent = modifyTVar' connVar $ \c -> c { status = Draining }

