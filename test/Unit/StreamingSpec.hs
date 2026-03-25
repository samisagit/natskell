{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module StreamingSpec where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import qualified Control.Monad
import           Data.ByteString
    ( ByteString
    , append
    , empty
    , hGetSome
    , hPut
    , head
    , isPrefixOf
    , length
    , pack
    , tail
    )
import qualified Data.ByteString.Lazy     as LBS
import           Data.Char                (chr)
import           Data.Word8               (isUpper)
import           GHC.IO.Handle
    ( BufferMode (NoBuffering)
    , Handle
    , hClose
    , hSetBuffering
    )
import           GHC.IO.IOMode
import           Lib.Logger
import           Lib.Parser
import           Network.ConnectionAPI    (ReaderAPI (..), WriterAPI (..))
import           Network.Socket           hiding (Debug)
import qualified Pipeline.Broadcasting    as Broadcast
import           Pipeline.Streaming       (run)
import           Prelude                  hiding
    ( head
    , last
    , length
    , null
    , replicate
    , tail
    , take
    )
import qualified Queue.API                as Queue
import           Queue.TransactionalQueue (QueueItem (..), newQ)
import           Test.Hspec
import qualified Types.Pub                as Pub

defaultLogger' :: IO LoggerConfig
defaultLogger' = do
  lock <- newTMVarIO ()
  pure $ LoggerConfig Debug (putStrLn . renderLogEntry) lock

bufferLimit :: Int
bufferLimit = 4096

newtype CaptureWriter = CaptureWriter (TVar [ByteString])

captureWriterApi :: WriterAPI CaptureWriter
captureWriterApi =
  WriterAPI
    { writerWriteData = \(CaptureWriter output) bytes -> do
        atomically $ modifyTVar' output (<> [bytes])
        pure (Right ())
    , writerWriteDataLazy = \writer bytes ->
        writerWriteData captureWriterApi writer (LBS.toStrict bytes)
    , writerClose = \_ -> pure ()
    , writerOpen = \_ -> pure ()
    }

handleReaderApi :: ReaderAPI Handle
handleReaderApi =
  ReaderAPI
    { readerReadData = \h n -> do
        result <- try (hGetSome h n) :: IO (Either SomeException ByteString)
        case result of
          Left err    -> return $ Left (show err)
          Right bytes -> return $ Right bytes
    , readerClose = hClose
    , readerOpen = \_ -> pure ()
    }

spec :: Spec
spec = do
  describe "Streaming" $ do
      it "reads from source and writes to sink" $ do
        (server, client) <- makeSocketPair
        result <- newTVarIO "" :: IO (TVar ByteString)
        let parser bs = Right (pack [head bs], tail bs) :: Either ParserErr (ByteString, ByteString)
        let sink curr = atomically $ modifyTVar' result (`append` curr)
        dl <- defaultLogger'
        ctx <- newLogContext
        (forkIO . runWithLogger dl ctx) (run bufferLimit handleReaderApi client parser sink :: AppM ())
        hPut server "Hello, World"
        atomically $ assertTVarWithRetry result "Hello, World"
        hClose server
        hClose client
      it "continues reading from the handle after reaching end" $ do
        (server, client) <- makeSocketPair
        result <- newTVarIO "" :: IO (TVar ByteString)
        let parser bs = Right (pack [head bs], tail bs) :: Either ParserErr (ByteString, ByteString)
        let sink curr = atomically $ modifyTVar' result (`append` curr)
        dl <- defaultLogger'
        ctx <- newLogContext
        (forkIO . runWithLogger dl ctx) (run bufferLimit handleReaderApi client parser sink :: AppM ())
        hPut server "Hello, World"
        atomically $ assertTVarWithRetry result "Hello, World"
        hPut server "Hello, again"
        atomically $ assertTVarWithRetry result "Hello, WorldHello, again"
        hClose server
        hClose client
      it "applies the parser healing" $ do
        (server, client) <- makeSocketPair
        result <- newTVarIO "" :: IO (TVar ByteString)
        let sink curr = atomically $ modifyTVar' result (`append` curr)
        dl <- defaultLogger'
        ctx <- newLogContext
        (forkIO . runWithLogger dl ctx) (run bufferLimit handleReaderApi client testParserExcludingUpper sink :: AppM ())
        hPut server "HELLO WORLD hello, world"
        atomically $ assertTVarWithRetry result "  hello, world"
        hClose server
        hClose client
      it "waits for more data" $ do
        (server, client) <- makeSocketPair
        result <- newTVarIO "" :: IO (TVar ByteString)
        let sink curr = atomically $ modifyTVar' result (`append` curr)
        dl <- defaultLogger'
        ctx <- newLogContext
        (forkIO . runWithLogger dl ctx) (run bufferLimit handleReaderApi client testParserForExplicitWord sink :: AppM ())
        hPut server "part1"
        threadDelay 100000
        atomically $ ensureTVarIsEmpty result
        hPut server "part2"
        atomically $ assertTVarWithRetry result "part1part2"
        hClose server
        hClose client
  describe "Broadcasting" $ do
      it "drops messages larger than the buffer limit" $ do
        dl <- defaultLogger'
        ctx <- newLogContext
        q <- newQ
        output <- newTVarIO []
        let pub = Pub.Pub "FOO" Nothing Nothing (Just "0123456789")
        Queue.enqueue q (QueueItem pub) `shouldReturn` Right ()
        Queue.close q
        runWithLogger dl ctx (Broadcast.run 5 q captureWriterApi (CaptureWriter output) :: AppM ())
        readTVarIO output `shouldReturn` []

ensureTVarIsEmpty :: TVar ByteString -> STM ()
ensureTVarIsEmpty tvar = do
  content <- readTVar tvar
  Control.Monad.when (content /= empty) retry

assertTVarWithRetry :: Eq a => TVar a -> a -> STM ()
assertTVarWithRetry tvar expected = do
  actual <- readTVar tvar
  Control.Monad.unless (actual == expected) retry

testParserExcludingUpper :: ByteString -> Either ParserErr (ByteString, ByteString)
testParserExcludingUpper bs = case isUpper . head $ bs of
                                False -> Right (pack [head bs], tail bs) ;
                                True -> Left (UnexpectedChar [chr .fromIntegral . head $ bs] (length bs))

testParserForExplicitWord :: ByteString -> Either ParserErr (ByteString, ByteString)
testParserForExplicitWord bs
  | bs == "part1part2" = Right (bs, empty)
  | bs `isPrefixOf` "part1part2" = Left (UnexpectedEndOfInput "test" (length bs))
  | otherwise          = Left (UnexpectedChar [chr .fromIntegral . head $ bs] 1)

makeSocketPair :: IO (Handle, Handle)
makeSocketPair = do
  serverSock <- socket AF_INET Stream defaultProtocol
  setSocketOption serverSock ReuseAddr 1
  bind serverSock (SockAddrInet 0 (tupleToHostAddress (127,0,0,1)))
  listen serverSock 1
  SockAddrInet port _ <- getSocketName serverSock

  -- initiate client connection
  clientSock <- socket AF_INET Stream defaultProtocol
  connect clientSock (SockAddrInet port (tupleToHostAddress (127,0,0,1)))

  -- accept connection from server side
  (serverConn, _) <- accept serverSock

  -- convert both sides to handles
  serverHandle <- socketToHandle serverConn ReadWriteMode
  clientHandle <- socketToHandle clientSock ReadWriteMode
  hSetBuffering serverHandle NoBuffering
  hSetBuffering clientHandle NoBuffering

  return (serverHandle, clientHandle)
