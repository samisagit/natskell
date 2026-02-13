module Pipeline.Streaming.Parser where
import           Conduit
import           Data.ByteString
import qualified Data.ByteString as BS
import           Lib.Logger
import           Lib.Parser
import           Prelude         hiding (drop, length, take)

parser :: (MonadLogger m , MonadIO m)
  => (ByteString -> Either ParserErr (result, ByteString))
  -> ConduitT ByteString result m ()
parser p = loop empty
  where
    loop acc = do
      bs <- await
      case bs of
        Nothing    -> return ()
        Just chunk -> handleChunk $ append acc chunk
    handleChunk bs
      | BS.null bs = parser p
      | otherwise = do
      lift . logMessage Debug $ "parsing chunk"
      case p bs of
        Left err -> do
          case solveErr err (length bs) of
            SuggestPull -> if length bs > 4096
              then do
              lift . logMessage Error $ "overloaded buffer"
              lift . logMessage Debug $ ("invalid prefix: " ++ show (take 4096 bs))
              handleChunk (drop 4096 bs)
              else do
                lift . logMessage Debug $ "message spans frame, waiting for more data"
                loop bs
            SuggestDrop n r -> do
              lift . logMessage Error $ ("dropping invalid prefix: " ++ r)
              lift . logMessage Debug $ ("invalid prefix: " ++ show bs)
              lift . logMessage Error $ ("dropping " ++ show n ++ " bytes")
              handleChunk (drop n bs)
        Right (message, rest) -> do
          lift . logMessage Debug $ "parsed message"
          yield message
          handleChunk rest
