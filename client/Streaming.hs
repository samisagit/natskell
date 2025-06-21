{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Streaming where

import           Conduit
import           Control.Concurrent (threadDelay)
import           Control.Exception  (IOException, try)
import           Data.ByteString    (ByteString, append, hGetSome, null)
import           Prelude            hiding (null)
import           System.IO
    ( BufferMode (NoBuffering)
    , Handle
    , hSetBuffering
    )
data ParserState = OK
                 | MissingData -- Reached end of input, content valid so far
                 | OverflowingData -- Reached max message size, valid so far
                 | InvalidPrefix -- Met error before end of input
                 | Drained -- There is nothing to consume and no partial message in memory
                 | StreamEnded

class SelfHealer value err where
  heal :: value -> err -> (value, ParserState)

runPipeline :: SelfHealer ByteString a
  => Show a
  => Handle                                   -- socket
  -> (ByteString -> Either a (b, ByteString)) -- parser
  -> (b -> IO ())                             -- router
  -> IO ()
runPipeline handle parser router = do
  hSetBuffering handle NoBuffering
  runConduitRes $
    handleSource handle .| streamParser parser .| ioSink router

handleSource :: MonadIO m
  => Handle -> ConduitT () ByteString m ()
handleSource handle = loop handle
  where
    loop handle = do
      result <- liftIO . try $ hGetSome handle 4096
      case result of
        Left (e :: IOException) -> do
          liftIO $ threadDelay 1000000
          loop handle
        Right chunk ->
          if null chunk
            then do
              liftIO $ threadDelay 1000000
              loop handle
            else yield chunk >> loop handle

streamParser :: SelfHealer ByteString err
  => Show err
  => MonadIO m
  => (ByteString -> Either err (result, ByteString)) -> ConduitT ByteString result m ()
streamParser parser = loop ""
  where
    loop acc = do
      mbChunk <- await
      case mbChunk of
        Nothing    -> loop acc
        Just chunk -> do
          let combined = acc `append` chunk
          handleChunk combined
    handleChunk "" = loop ""
    handleChunk bs = do
      case parser bs of
        Left err -> do
          liftIO . putStrLn $ "Parser error: " ++ show err
          let (fixed, state) = heal bs err
          -- TODO: if the heal failed, we shouldn't just continue
          handleChunk fixed
        Right (message, rest) -> do
          yield message
          handleChunk rest

ioSink :: MonadIO m => (a -> IO ()) -> ConduitT a Void m ()
ioSink action = loop
  where
    loop = do
      ma <- await
      case ma of
        Nothing -> return () -- TODO: need to decide if we want the sink to terminate the stream
        Just a  -> do
          liftIO . action $ a
          loop

