{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Pipeline.Streaming where

import           Conduit
import           Control.Concurrent     (threadDelay)
import           Control.Concurrent.STM
import           Control.Exception      (IOException, try)
import           Control.Monad
import           Data.ByteString
    ( ByteString
    , append
    , drop
    , hGetSome
    , length
    , null
    , take
    )
import           GHC.Conc               (forkIO)
import           Lib.Logger
import           Lib.Parser
import           Prelude                hiding (drop, length, null, take)
import           System.IO
    ( BufferMode (NoBuffering)
    , Handle
    , hSetBuffering
    )

runPipeline :: (MonadLogger m , MonadIO m)
  => Handle                                           -- socket
  -> (ByteString -> Either ParserErr (b, ByteString)) -- parser
  -> (b -> IO ())                                     -- router
  -> TVar Bool                                        -- poison pill
  -> m ()
runPipeline handle parser router poisonpill = do
  liftIO $ hSetBuffering handle NoBuffering
  runConduit $ handleSource handle poisonpill .| streamParser parser .| ioSink router

handleSource :: (MonadLogger m , MonadIO m)
  => Handle
  -> TVar Bool
  -> ConduitT () ByteString m ()
handleSource handle poisonpill = do
  s <- liftIO $ readTVarIO poisonpill
  case s of
    True -> lift . logDebug $ "poison pill received, stopping reader"
    _ -> do
           lift . logDebug $ "reading from socket"
           result <- liftIO . try $ hGetSome handle 4096
           case result of
             Left (e :: IOException) -> do
               lift . logFatal $ "IOException: " ++ show e
               liftIO . atomically $ writeTVar poisonpill True
               return ()
             Right chunk ->
               if null chunk
                 then do
                   lift . logDebug $ "end of stream reached, waiting for more data"
                   liftIO $ threadDelay 1000000
                   handleSource handle poisonpill
                 else yield chunk >> (lift . logDebug $ "read from socket") >> handleSource handle poisonpill

streamParser :: (MonadLogger m , MonadIO m)
  => (ByteString -> Either ParserErr (result, ByteString))
  -> ConduitT ByteString result m ()
streamParser parser = loop ""
  where
    loop acc = do
      bs <- await
      case bs of
        Nothing    -> return ()
        Just chunk -> handleChunk $ append acc chunk
    handleChunk "" = streamParser parser
    handleChunk bs = do
      lift . logDebug $ "parsing chunk"
      case parser bs of
        Left err -> do
          case solveErr err of
            SuggestPull -> if length bs > 4096
              then do
              lift . logWarn $ "overloaded buffer"
              lift . logDebug $ ("invalid prefix: " ++ show (take 4096 bs))
              handleChunk (drop 4096 bs)
              else do
                lift . logDebug $ "message spans frame, waiting for more data"
                loop bs
            SuggestDrop n r -> do
              lift . logWarn $ ("dropping invalid prefix: " ++ r)
              lift . logDebug $ ("invalid prefix: " ++ show bs)
              handleChunk (drop n bs)
        Right (message, rest) -> do
          lift . logDebug $ "parsed message"
          yield message
          handleChunk rest

ioSink :: (MonadIO m, MonadLogger m) => (a -> IO ()) -> ConduitT a Void m ()
ioSink action = do
  awaitForever $ \ma -> do
    liftIO . void . forkIO $ action ma
    ioSink action
    lift . logDebug $ "executed action on message"

