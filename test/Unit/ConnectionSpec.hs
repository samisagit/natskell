{-# LANGUAGE OverloadedStrings #-}

module ConnectionSpec (spec) where

import           Auth.None                 (auth)
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Handshake.Nats            (performHandshake)
import           Lib.Logger
    ( LogLevel (Debug)
    , LoggerConfig (LoggerConfig)
    , newLogContext
    )
import           Network.Connection        (connectionApi)
import           Network.Connection.Core   (Transport (..), pointTransport)
import           Network.ConnectionAPI
    ( closeReader
    , newConn
    , readData
    , reader
    )
import           Parser.API
    ( ParseStep (DropPrefix, Emit, NeedMore, Reject)
    , ParsedMessage (ParsedInfo)
    , ParserAPI (ParserAPI)
    )
import           Queue.TransactionalQueue  (newQueue)
import           State.Store               (newClientState, readServerInfo)
import           State.Types               (ClientConfig (..))
import           System.Timeout            (timeout)
import           Test.Hspec
import           Transformers.Transformers (Transformer (transform))
import qualified Types.Connect             as Connect
import           Types.Info                (Info (Info))

spec :: Spec
spec = do
  describe "Connection reader" $ do
    it "unblocks a blocking read when closeReader is called" $ do
      conn <- newConn connectionApi
      started <- newEmptyMVar
      blocker <- (newEmptyMVar :: IO (MVar BS.ByteString))
      let transport = Transport
            { transportRead = \_ -> putMVar started () >> takeMVar blocker
            , transportWrite = \_ -> pure ()
            , transportWriteLazy = \_ -> pure ()
            , transportFlush = pure ()
            , transportClose = pure ()
            , transportUpgrade = Nothing
            }
      pointTransport conn transport
      resultVar <- newEmptyMVar
      _ <- forkIO $ readData (reader connectionApi) conn 1 >>= putMVar resultVar
      _ <- takeMVar started
      closeReader (reader connectionApi) conn
      result <- timeout 1000000 (takeMVar resultVar)
      result `shouldBe` Just (Left "Read operation is blocked")
  describe "Handshake" $ do
    it "accepts an incremental parser backend for the initial INFO frame" $ do
      state <- newTestState
      conn <- newConn connectionApi
      writes <- newTVarIO []
      transport <-
        newScriptedTransport
          [ "INF"
          , "O {\"server_id\":\"srv\",\"version\":\"1.0.0\",\"go\":\"go1\",\"host\":\"127.0.0.1\",\"port\":4222,\"max_payload\":1024,\"proto\":1}\r\n"
          ]
          writes
      pointTransport conn transport

      result <- performHandshake connectionApi incrementalInfoParser state auth conn "127.0.0.1"

      result `shouldBe` Right ()
      readServerInfo state `shouldReturn` Just testInfo
      readTVarIO writes `shouldReturn` [LBS.toStrict (transform testConnect)]

    it "accepts a parser backend that drops an invalid prefix before INFO" $ do
      state <- newTestState
      conn <- newConn connectionApi
      writes <- newTVarIO []
      transport <-
        newScriptedTransport
          [ "XIN"
          , "FO {\"server_id\":\"srv\",\"version\":\"1.0.0\",\"go\":\"go1\",\"host\":\"127.0.0.1\",\"port\":4222,\"max_payload\":1024,\"proto\":1}\r\n"
          ]
          writes
      pointTransport conn transport

      result <- performHandshake connectionApi dropPrefixInfoParser state auth conn "127.0.0.1"

      result `shouldBe` Right ()
      readServerInfo state `shouldReturn` Just testInfo
      readTVarIO writes `shouldReturn` [LBS.toStrict (transform testConnect)]

newTestState = do
  queue <- newQueue
  ctx <- newLogContext
  conn <- newConn connectionApi
  logger <- newSilentLogger
  newClientState (testConfig logger) queue conn ctx

newSilentLogger :: IO LoggerConfig
newSilentLogger = do
  lock <- newTMVarIO ()
  pure (LoggerConfig Debug (\_ -> pure ()) lock)

testConfig :: LoggerConfig -> ClientConfig
testConfig logger =
  ClientConfig
    { connectionAttempts = 1
    , callbackConcurrency = 1
    , bufferLimit = 4096
    , connectConfig = testConnect
    , loggerConfig = logger
    , tlsCert = Nothing
    , exitAction = const (pure ())
    , connectOptions = []
    }

testConnect :: Connect.Connect
testConnect =
  Connect.Connect
    { Connect.verbose = False
    , Connect.pedantic = True
    , Connect.tls_required = False
    , Connect.auth_token = Nothing
    , Connect.user = Nothing
    , Connect.pass = Nothing
    , Connect.name = Nothing
    , Connect.lang = "haskell"
    , Connect.version = "0.1.0"
    , Connect.protocol = Nothing
    , Connect.echo = Just True
    , Connect.sig = Nothing
    , Connect.jwt = Nothing
    , Connect.nkey = Nothing
    , Connect.no_responders = Just True
    , Connect.headers = Just True
    }

testInfo :: Info
testInfo =
  Info
    "srv"
    "1.0.0"
    "go1"
    "127.0.0.1"
    4222
    1024
    1
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

incrementalInfoParser :: ParserAPI ParsedMessage
incrementalInfoParser = ParserAPI parseInfo
  where
    parseInfo bytes
      | bytes == "INF" =
          NeedMore
      | bytes == infoFrame =
          Emit (ParsedInfo testInfo) ""
      | otherwise =
          Reject ("unexpected input: " ++ show bytes)

dropPrefixInfoParser :: ParserAPI ParsedMessage
dropPrefixInfoParser = ParserAPI parseInfo
  where
    parseInfo bytes
      | bytes == "XIN" =
          DropPrefix 1 "invalid prefix"
      | bytes == infoFrame =
          Emit (ParsedInfo testInfo) ""
      | otherwise =
          Reject ("unexpected input: " ++ show bytes)

infoFrame :: BS.ByteString
infoFrame =
  "INFO {\"server_id\":\"srv\",\"version\":\"1.0.0\",\"go\":\"go1\",\"host\":\"127.0.0.1\",\"port\":4222,\"max_payload\":1024,\"proto\":1}\r\n"

newScriptedTransport :: [BS.ByteString] -> TVar [BS.ByteString] -> IO Transport
newScriptedTransport chunks writes = do
  remainingChunks <- newTVarIO chunks
  pure $
    Transport
      { transportRead = \_ ->
          atomically $ do
            remaining <- readTVar remainingChunks
            case remaining of
              nextChunk:rest -> do
                writeTVar remainingChunks rest
                pure nextChunk
              [] ->
                pure BS.empty
      , transportWrite = \bytes ->
          atomically $ modifyTVar' writes (<> [bytes])
      , transportWriteLazy = \bytes ->
          atomically $ modifyTVar' writes (<> [LBS.toStrict bytes])
      , transportFlush = pure ()
      , transportClose = pure ()
      , transportUpgrade = Nothing
      }
