{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pipeline.Operator (newConnection, setStatus, run) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.ByteString
import           GHC.Conc               (forkIO)
import           GHC.IO.Handle
import           Lib.CallOption
import           Lib.Logger
import           Lib.Parser
import qualified Pipeline.Broadcasting  as B
import           Pipeline.Status
import qualified Pipeline.Streaming     as S


data Connection = Connection
                    { status :: TVar Status
                    , socket :: Handle
                    }

type ConnectionOpts = CallOption Connection

newConnection :: Handle -> [ConnectionOpts] -> IO Connection
newConnection handle opts = do
  status <- newTVarIO Connecting
  let conn = Connection {
    status = status
    , socket = handle
  }
  return $ applyCallOptions opts conn

setStatus :: Connection -> Status -> IO ()
setStatus conn newStatus = atomically $ writeTVar (status conn) newStatus

run :: (MonadLogger m , MonadIO m, MonadReader LoggerConfig m)
  => Connection
  -> (ByteString -> Either ParserErr (b, ByteString))
  -> (b -> IO ())
  -> TBQueue B.QueueItem
  -> m ()
run conn parser router queue = do
  cfg <- ask
  liftIO $ do
    (void . forkIO) . runWithLogger cfg $ S.runPipeline (socket conn) parser router (status conn)
    (void . forkIO) . runWithLogger cfg $ B.runPipeline queue (socket conn) (status conn)

