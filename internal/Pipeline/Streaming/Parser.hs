module Pipeline.Streaming.Parser where
import           Conduit
import           Data.ByteString
import qualified Data.ByteString  as BS
import           Lib.Logger.Types (LogLevel (..), MonadLogger (..))
import           Parser
import           Prelude          hiding (drop, length, take)

parser :: (MonadLogger m , MonadIO m)
  => Int
  -> (ByteString -> Either ParserErr (result, ByteString))
  -> ConduitT ByteString result m ()
parser bufferLimit p = loop empty
  where
    loop acc = do
      bs <- await
      case bs of
        Nothing    -> return ()
        Just chunk -> handleChunk $ append acc chunk
    handleChunk bs
      | BS.null bs = parser bufferLimit p
      | otherwise = do
          lift . logMessage Debug $ "parsing chunk"
          case p bs of
            Left err -> do
              let bsLen = length bs
              case solveErr err bsLen of
                SuggestPull ->
                  if bsLen > bufferLimit
                    then do
                      lift . logMessage Error $
                        "overloaded buffer: "
                          ++ show bsLen
                          ++ " bytes exceeds limit "
                          ++ show bufferLimit
                      lift . logMessage Debug $ ("invalid prefix: " ++ show (take bufferLimit bs))
                      handleChunk (drop bufferLimit bs)
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
              let consumedBytes = length bs - length rest
              if consumedBytes > bufferLimit
                then do
                  lift . logMessage Error $
                    "dropping inbound message: "
                      ++ show consumedBytes
                      ++ " bytes exceeds limit "
                      ++ show bufferLimit
                  handleChunk rest
                else do
                  yield message
                  handleChunk rest
