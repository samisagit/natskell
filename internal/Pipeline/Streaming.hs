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
import           Data.ByteString        (ByteString, append, drop, hGetSome,
                                         length, null)
import           GHC.Conc               (forkIO)
import           Lib.Parser
import           Pipeline.Status
import           Prelude                hiding (drop, length, null)
import           System.IO              (BufferMode (NoBuffering), Handle,
                                         hSetBuffering)

-- TODO: either inject a logger or make this monadic
debug :: MonadIO m => String -> m ()
debug msg = liftIO . putStrLn $ ("WARN: " ++ msg)

-- TODO: either inject a logger or make this monadic
warn :: MonadIO m => String -> m ()
warn msg = liftIO . putStrLn $ ("WARN: " ++ msg)


runPipeline ::
  Handle                                              -- socket
  -> (ByteString -> Either ParserErr (b, ByteString)) -- parser
  -> (b -> IO ())                                     -- router
  -> TVar Status
  -> IO ()
runPipeline handle parser router status = do
  hSetBuffering handle NoBuffering
  runConduitRes $
    handleSource handle status .| streamParser parser .| ioSink router

handleSource :: MonadIO m
  => Handle
  -> TVar Status
  -> ConduitT () ByteString m ()
handleSource handle status = do
  s <- liftIO $ readTVarIO status
  case s of
    Disconnected _ -> return ()
    Disconnecting _ -> return ()
    Draining -> return ()
    _ -> do
           result <- liftIO . try $ hGetSome handle 4096
           case result of
             Left (e :: IOException) -> do
               warn . show $ e
               liftIO . atomically $ writeTVar status (Disconnected (show e))
               return ()
             Right chunk ->
               if null chunk
                 then do
                   debug "end of stream reached, waiting for more data"
                   liftIO $ threadDelay 1000000
                   handleSource handle status
                 else yield chunk >> handleSource handle status

streamParser :: MonadIO m
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
      case parser bs of
        Left err -> do
          case solveErr err of
            SuggestPull -> if length bs > 4096
              then warn "overloaded buffer" >> handleChunk (drop 4096 bs)
              else debug "message spans frame" >> loop bs
            SuggestDrop n r -> do
              warn $ "dropping invalid prefix: " ++ r
              debug $ "invalid prefix: " ++ show bs
              handleChunk (drop n bs)
        Right (message, rest) -> do
          yield message
          handleChunk rest

ioSink :: MonadIO m => (a -> IO ()) -> ConduitT a Void m ()
ioSink action = do
  awaitForever $ \ma -> do
    liftIO . void . forkIO $ action ma
    ioSink action
  
