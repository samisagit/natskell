{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module StreamingSpec where

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Monad
import           Data.ByteString
    ( ByteString
    , append
    , hPut
    , head
    , pack
    , tail
    )
import           Data.Char              (chr)
import           Data.Word8             (isUpper)
import           Pipeline.Streaming
import           Prelude                hiding
    ( head
    , last
    , null
    , replicate
    , tail
    )
import           System.IO
    ( Handle
    , SeekMode (AbsoluteSeek)
    , hClose
    , hSeek
    , hTell
    , openBinaryTempFile
    )
import           Test.Hspec

-- DISCLAIMER: These tests use a temp file to simulate a socket, which means when the end of the file is reached we get EOF.
-- This causes behaviour most similar to a socket closing from the server end, which wouldn't happen in the happy path of a real scenario.
-- The conduit code is written to handle this case, but it means in the context of the test there is a lot of polling going on when there
-- is no more content to consume

instance SelfHealer ByteString String where
  heal bs _  = (tail bs, OK)

spec :: Spec
spec = do
  describe "Streaming" $ do
      it "reads from handle" $ do
        (handle, _) <- byteStringToTempHandle "Hello, World"
        result <- newTVarIO "" :: IO (TVar ByteString)
        let parser bs = Right (pack [head bs], tail bs) :: Either String (ByteString, ByteString)
        let sink curr = atomically $ modifyTVar' result (`append` curr)
        forkIO . runPipeline handle parser $ sink
        atomically $ assertTVarWithRetry result "Hello, World"
        hClose handle
      it "continues reading from the handle after reaching end" $ do
        (handle, _) <- byteStringToTempHandle "Hello, World"
        result <- newTVarIO "" :: IO (TVar ByteString)
        let parser bs = Right (pack [head bs], tail bs) :: Either String (ByteString, ByteString)
        let sink curr = atomically $ modifyTVar' result (`append` curr)
        forkIO . runPipeline handle parser $ sink
        atomically $ assertTVarWithRetry result "Hello, World"
        writeToTestHandle "Hello, again" handle
        atomically $ assertTVarWithRetry result "Hello, WorldHello, again"
        hClose handle
      it "applies the naive parser healing" $ do
        (handle, _) <- byteStringToTempHandle "HELLO WORLD hello, world"
        result <- newTVarIO "" :: IO (TVar ByteString)
        let sink curr = atomically $ modifyTVar' result (`append` curr)
        forkIO . runPipeline handle testParserExcludingUpper $ sink
        atomically $ assertTVarWithRetry result "  hello, world"
        hClose handle

assertTVarWithRetry :: Eq a => TVar a -> a -> STM ()
assertTVarWithRetry tvar expected = do
  actual <- readTVar tvar
  Control.Monad.unless (actual == expected) retry

testParserExcludingUpper :: ByteString -> Either String (ByteString, ByteString)
testParserExcludingUpper bs = case isUpper . head $ bs of False -> Right (pack [head bs], tail bs) ; True -> Left ("upper case text not allowed: found " ++ [chr .fromIntegral . head $ bs])

byteStringToTempHandle :: ByteString -> IO (Handle, FilePath)
byteStringToTempHandle bs = do
  (fp, handle) <- openBinaryTempFile "/tmp" "tempfile"
  writeToTestHandle bs handle
  return (handle, fp)

writeToTestHandle :: ByteString -> Handle -> IO ()
writeToTestHandle bs handle = do
  pos <- hTell handle
  hPut handle bs
  hSeek handle AbsoluteSeek pos

