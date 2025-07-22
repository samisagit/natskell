{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pipeline.Operator (run) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.ByteString
import           GHC.Conc               (forkIO)
import           GHC.IO.Handle
import           Lib.Logger
import           Lib.Parser
import qualified Pipeline.Broadcasting  as B
import qualified Pipeline.Streaming     as S
import           WaitGroup

run :: (MonadLogger m , MonadIO m, MonadReader LoggerConfig m)
  => Handle
  -> TVar Bool
  -> (ByteString -> Either ParserErr (b, ByteString))
  -> (b -> IO ())
  -> TBQueue B.QueueItem
  -> m ()
run conn poisonpill parser router queue = do
  cfg <- ask
  wg <- liftIO . newWaitGroup $ 2
  liftIO $ do
    ((void . forkIO) . runWithLogger cfg $ S.runPipeline conn parser router poisonpill) >> done wg
    ((void . forkIO) . runWithLogger cfg $ B.runPipeline queue conn poisonpill) >> done wg
  liftIO . wait $ wg

