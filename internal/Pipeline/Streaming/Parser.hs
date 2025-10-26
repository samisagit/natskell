{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Streaming.Parser where
import           Conduit
import           Data.ByteString
import           Lib.Logger
import           Lib.Parser
import           Prelude         hiding (drop, length, take)

parser :: (MonadLogger m , MonadIO m)
  => (ByteString -> Either ParserErr (result, ByteString))
  -> ConduitT ByteString result m ()
parser p = loop ""
  where
    loop acc = do
      bs <- await
      case bs of
        Nothing    -> return ()
        Just chunk -> handleChunk $ append acc chunk
    handleChunk "" = parser p
    handleChunk bs = do
      lift . logDebug $ "parsing chunk"
      case p bs of
        Left err -> do
          case solveErr err of
            SuggestPull -> if length bs > 4096
              then do
              lift . logError $ "overloaded buffer"
              lift . logDebug $ ("invalid prefix: " ++ show (take 4096 bs))
              handleChunk (drop 4096 bs)
              else do
                lift . logDebug $ "message spans frame, waiting for more data"
                loop bs
            SuggestDrop n r -> do
              lift . logError $ ("dropping invalid prefix: " ++ r)
              lift . logDebug $ ("invalid prefix: " ++ show bs)
              handleChunk (drop n bs)
        Right (message, rest) -> do
          lift . logDebug $ "parsed message"
          yield message
          handleChunk rest
