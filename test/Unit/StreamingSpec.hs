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
import           GHC.IO.Handle
    ( BufferMode (NoBuffering)
    , Handle
    , hClose
    , hSetBuffering
    )
import           GHC.IO.IOMode
import           Network.Socket
import           Pipeline.Streaming
import           Prelude                hiding
    ( head
    , last
    , null
    , replicate
    , tail
    )
import           Test.Hspec

instance SelfHealer ByteString String where
  heal bs _  = (tail bs, OK)

spec :: Spec
spec = do
  describe "Streaming" $ do
      it "reads from handle" $ do
        (server, client) <- makeSocketPair
        result <- newTVarIO "" :: IO (TVar ByteString)
        let parser bs = Right (pack [head bs], tail bs) :: Either String (ByteString, ByteString)
        let sink curr = atomically $ modifyTVar' result (`append` curr)
        forkIO . runPipeline client parser $ sink
        hPut server "Hello, World"
        atomically $ assertTVarWithRetry result "Hello, World"
        hClose server
        hClose client
      it "continues reading from the handle after reaching end" $ do
        (server, client) <- makeSocketPair
        result <- newTVarIO "" :: IO (TVar ByteString)
        let parser bs = Right (pack [head bs], tail bs) :: Either String (ByteString, ByteString)
        let sink curr = atomically $ modifyTVar' result (`append` curr)
        forkIO . runPipeline client parser $ sink
        hPut server "Hello, World"
        atomically $ assertTVarWithRetry result "Hello, World"
        hPut server "Hello, again"
        atomically $ assertTVarWithRetry result "Hello, WorldHello, again"
        hClose server
        hClose client
      it "applies the naive parser healing" $ do
        (server, client) <- makeSocketPair
        result <- newTVarIO "" :: IO (TVar ByteString)
        let sink curr = atomically $ modifyTVar' result (`append` curr)
        forkIO . runPipeline client testParserExcludingUpper $ sink
        hPut server "HELLO WORLD hello, world"
        atomically $ assertTVarWithRetry result "  hello, world"
        hClose server
        hClose client

assertTVarWithRetry :: Eq a => TVar a -> a -> STM ()
assertTVarWithRetry tvar expected = do
  actual <- readTVar tvar
  Control.Monad.unless (actual == expected) retry

testParserExcludingUpper :: ByteString -> Either String (ByteString, ByteString)
testParserExcludingUpper bs = case isUpper . head $ bs of False -> Right (pack [head bs], tail bs) ; True -> Left ("upper case text not allowed: found " ++ [chr .fromIntegral . head $ bs])

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
