{-# LANGUAGE MultiParamTypeClasses #-}
module Pipeline.Operator (newConnection,  withLogger, run) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.ByteString
import           GHC.Conc               (forkIO)
import           GHC.IO.Handle
import           Lib.CallOption
import           Lib.Logger             (Logger)
import           Lib.Parser
import qualified Pipeline.Broadcasting  as B
import           Pipeline.Status
import qualified Pipeline.Streaming     as S

data Connection = Connection
                    { status :: TVar Status
                    , socket :: Handle
                    , logger :: Maybe Logger
                    }

type ConnectionOpts = CallOption Connection

withLogger :: Logger -> ConnectionOpts
withLogger logger conn = conn { logger = Just logger }

newConnection :: Handle -> [ConnectionOpts] -> IO Connection
newConnection handle opts = do
  status <- newTVarIO Connecting
  let conn = Connection {
    status = status
    , socket = handle
    , logger = Nothing
  }
  return $ applyCallOptions opts conn

run ::
  Connection
  -> (ByteString -> Either ParserErr (b, ByteString))
  -> (b -> IO ())
  -> TBQueue B.QueueItem
  -> IO ()
run conn parser router queue = do
  void . forkIO $ S.runPipeline (socket conn) parser router (status conn)
  void . forkIO $ B.runPipeline queue (socket conn) (status conn)

