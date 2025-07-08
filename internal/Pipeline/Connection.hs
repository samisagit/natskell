{-# LANGUAGE GADTs #-}

module Pipeline.Connection where
import           Control.Concurrent.STM
import           Data.ByteString
import           GHC.IO.Handle
import           Transformers.Transformers

type ConnectionState = String

healthy :: Connection a b -> STM ()
healthy c = modifyTVar (state c) $ const "HEALTHY"

draining :: Connection a b -> STM ()
draining c = modifyTVar (state c) $ const "DRAINING"

disconnecting :: Connection a b -> STM ()
disconnecting c = modifyTVar (state c) $ const "DISCONNECTING"

disconnected :: Connection a b  -> STM ()
disconnected c = modifyTVar (state c) $ const "DISCONNECTED"

data QueueItem = forall m. Transformer m => QueueItem m

data Connection a b = Connection
                        { h      :: Handle
                        , parser :: ByteString -> Either a (b, ByteString)
                        , router :: b -> IO ()
                        , state  :: TVar ConnectionState
                        , queue  :: TBQueue QueueItem
                        }

