module ConnectionSpec (spec) where

import           Control.Concurrent
import qualified Data.ByteString         as BS
import           Network.Connection.Core
    ( Transport (..)
    , newConn
    , pointTransport
    )
import           Network.ConnectionAPI
    ( ReaderAPI (..)
    , connectionApi
    , connectionReaderApi
    )
import           System.Timeout          (timeout)
import           Test.Hspec

spec :: Spec
spec = describe "Connection reader" $ do
  it "unblocks a blocking read when closeReader is called" $ do
    conn <- newConn
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
    _ <- forkIO $ readerReadData (connectionReaderApi connectionApi) conn 1 >>= putMVar resultVar
    _ <- takeMVar started
    readerClose (connectionReaderApi connectionApi) conn
    result <- timeout 1000000 (takeMVar resultVar)
    result `shouldBe` Just (Left "Read operation is blocked")
