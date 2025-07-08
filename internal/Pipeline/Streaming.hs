{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Pipeline.Streaming where

import           Conduit
import           Control.Concurrent  (threadDelay)
import           Control.Exception   (IOException, try)
import           Data.ByteString     (ByteString, append, hGetSome, null)
import           Pipeline.Connection
import           Prelude             hiding (null)
import           System.IO           (BufferMode (NoBuffering), hSetBuffering)

data ParserState = OK
                 | MissingData -- Reached end of input, content valid so far
                 | OverflowingData -- Reached max message size, valid so far
                 | InvalidPrefix -- Met error before end of input
                 | Drained -- There is nothing to consume and no partial message in memory
                 | StreamEnded

class SelfHealer value err where
  heal :: value -> err -> (value, ParserState)

runPipeline :: SelfHealer ByteString err
  => Show err
  => Connection err result
  -> IO ()
runPipeline c = do
  hSetBuffering (h c) NoBuffering
  runConduitRes $ handleSource c .| streamParser c .| ioSink c

handleSource :: MonadIO m
  => Connection err result
  -> ConduitT () ByteString m ()
handleSource conn = loop handle
  where
    handle = h conn
    loop h = do
      result <- liftIO . try $ hGetSome h 4096
      case result of
        Left (e :: IOException) -> do
          liftIO . putStrLn $ ("Error reading from handle: " ++ show e)
          liftIO $ threadDelay 1000000
          loop h
        Right chunk ->
          if null chunk
            then do
              liftIO $ threadDelay 1000000
              loop h
            else yield chunk >> loop h

streamParser :: SelfHealer ByteString err
  => Show err
  => MonadIO m
  => Connection err result
  -> ConduitT ByteString result m ()
streamParser conn = loop ""
  where
    p = parser conn
    loop acc = do
      mbChunk <- await
      case mbChunk of
        Nothing    -> loop acc
        Just chunk -> do
          let combined = acc `append` chunk
          handleChunk combined
    handleChunk "" = loop ""
    handleChunk bs = do
      case p bs of
        Left err -> do
          liftIO . putStrLn $ "Parser error: " ++ show err
          let (fixed, state) = heal bs err
          -- TODO: if the heal failed, we shouldn't just continue
          handleChunk fixed
        Right (message, rest) -> do
          yield message
          handleChunk rest

ioSink :: MonadIO m
  => Connection err result
  -> ConduitT result Void m ()
ioSink conn = loop
  where
    action = router conn
    loop = do
      ma <- await
      case ma of
        Nothing -> return () -- TODO: need to decide if we want the sink to terminate the stream
        Just a  -> do
          liftIO . action $ a
          loop

