module Pipeline.Streaming.Parser where
import           Conduit
import           Data.ByteString
import qualified Data.ByteString  as BS
import           Lib.Logger.Types (LogLevel (..), MonadLogger (..))
import           Parser.API
    ( ParseStep (DropPrefix, Emit, NeedMore, Reject)
    , ParserAPI
    , parse
    )
import           Prelude          hiding (drop, length, take)

parser :: (MonadLogger m , MonadIO m)
  => Int
  -> ParserAPI result
  -> ConduitT ByteString result m ()
parser bufferLimit parserApi = loop empty
  where
    loop acc = do
      bs <- await
      case bs of
        Nothing    -> return ()
        Just chunk -> handleChunk $ append acc chunk
    handleChunk bs
      | BS.null bs = parser bufferLimit parserApi
      | otherwise = do
          lift . logMessage Debug $ "parsing chunk"
          case parse parserApi bs of
            NeedMore -> do
              let bsLen = length bs
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
            DropPrefix n reason -> do
              lift . logMessage Error $ ("dropping invalid prefix: " ++ reason)
              lift . logMessage Debug $ ("invalid prefix: " ++ show bs)
              lift . logMessage Error $ ("dropping " ++ show n ++ " bytes")
              handleChunk (drop n bs)
            Reject reason ->
              lift . logMessage Error $ ("parser rejected inbound data: " ++ reason)
            Emit message rest -> do
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
