{-# LANGUAGE OverloadedStrings #-}

module ClientSpec where

import           Client
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString           as BS
import           Data.Word8
import           Network.Socket            (Socket, accept, close, listen)
import           Network.Socket.ByteString (sendAll)
import           Network.Socket.Free
import           System.Timeout            (timeout)
import           Test.Hspec
import           WaitGroup

defaultINFO = "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3}\r\n"

tooLongMSG = "MSG a b 5000\r\n" <> BS.replicate 5000 _x <> "\r\n"

startClient :: IO (Socket, Client, Socket, TMVar ClientExitReason)
startClient = do
  (p, sock) <- openFreePort
  listen sock 1
  tva <- newEmptyTMVarIO
  void . forkIO $ do
    (serv, _) <- accept sock
    atomically $ putTMVar tva serv
  exited <- newEmptyTMVarIO
  tvb <- newEmptyTMVarIO
  void . forkIO $ do
    c <- newClient [("127.0.0.1", p)] [withExitAction (atomically . putTMVar exited), withConnectionAttempts 1, withConnectName "test-client"]
    atomically $ putTMVar tvb c

  s <- atomically $ takeTMVar tva
  sendAll s defaultINFO
  c <- atomically $ takeTMVar tvb
  return (s, c, sock, exited)

stopClient :: (Socket, Client, Socket, TMVar ClientExitReason) -> IO ()
stopClient (s, c, sock, _) = do
  Client.close c
  Network.Socket.close sock
  Network.Socket.close s

withClient :: ((Socket, Client, Socket, TMVar ClientExitReason) -> IO()) -> IO ()
withClient action = do
  bracket startClient stopClient action

spec :: Spec
spec = do
  describe "client integration" $ do
    around withClient $ do
      it "PING waits for PONG" $ \(serv, client, _, _) -> do
        wg <- newWaitGroup 1
        ping client $ done wg
        sendAll serv "PONG\r\n"
        wait wg
      it "flush waits for PONG" $ \(serv, client, _, _) -> do
        doneVar <- newEmptyMVar
        void . forkIO $ do
          flush client
          putMVar doneVar ()
        threadDelay 100000
        returnedEarly <- not <$> isEmptyMVar doneVar
        when returnedEarly $
          expectationFailure "flush returned before PONG"
        sendAll serv "PONG\r\n"
        result <- timeout 1000000 (takeMVar doneVar)
        case result of
          Nothing -> expectationFailure "flush did not return after PONG"
          Just () -> pure ()
      it "PONG resolves one ping" $ \(serv, client, _, _) -> do
        first <- newEmptyMVar
        second <- newEmptyMVar
        ping client (putMVar first ())
        ping client (putMVar second ())
        sendAll serv "PONG\r\n"
        firstResult <- timeout 1000000 (takeMVar first)
        case firstResult of
          Nothing -> expectationFailure "first ping did not resolve"
          Just () -> pure ()
        threadDelay 100000
        secondReady <- not <$> isEmptyMVar second
        when secondReady $
          expectationFailure "second ping resolved before second PONG"
        sendAll serv "PONG\r\n"
        secondResult <- timeout 1000000 (takeMVar second)
        case secondResult of
          Nothing -> expectationFailure "second ping did not resolve"
          Just () -> pure ()
      it "reports user initiated close" $ \(_, client, _, exited) -> do
        Client.close client
        result <- atomically $ readTMVar exited
        result `shouldBe` ExitClosedByUser
      it "fatal error results in disconnect" $ \(serv, _, _, exited) -> do
        sendAll serv "-ERR 'Unknown Protocol Operation'\r\n"
        result <- atomically $ readTMVar exited
        case result of
          ExitServerError _ -> return ()
          other             -> expectationFailure $ "Unexpected exit reason: " ++ show other
      it "non fatal error does not result in disconnect" $ \(serv, client, _, _) -> do
        sendAll serv "-ERR 'Invalid Subject'\r\n"
        wg <- newWaitGroup 1
        ping client $ done wg
        sendAll serv "PONG\r\n"
        wait wg
      it "garbled prefix bytes are ignored" $ \(serv, client, _, _) -> do
        wg <- newWaitGroup 1
        ping client $ done wg
        sendAll serv "ldkfjajhfklsjhlkajf;alwfPONG\r\n"
        wait wg
      it "garbled suffix bytes remove partial prefix" $ \(serv, client, _, _) -> do
        wg <- newWaitGroup 1
        ping client $ done wg
        sendAll serv "MSGX"
        sendAll serv "PONG\r\n"
        wait wg
      it "messages split over frames are joined" $ \(serv, client, _, _) -> do
        wg <- newWaitGroup 1
        ping client $ done wg
        sendAll serv "PON"
        threadDelay 100000
        sendAll serv "G\r\n"
        wait wg
      it "exits when server goes away" $ \(serv, _, _, exited) -> do
        Network.Socket.close serv
        result <- atomically $ readTMVar exited
        case result of
          ExitRetriesExhausted _ -> return ()
          other                  -> expectationFailure $ "Unexpected exit reason: " ++ show other
      it "drops messages too long for processing" $ \(serv, client, _, _) -> do
        wg <- newWaitGroup 1
        ping client $ done wg
        sendAll serv tooLongMSG
        sendAll serv "PONG\r\n"
        wait wg
      it "unsubscribes after timeout" $ \(_, client, _, _) -> do
        wg <- newWaitGroup 1
        _ <- request client "foo" [withSubscriptionExpiry 1] (\x -> do
          case x of
            Nothing -> done wg
            Just _  -> error "should not receive message"
          )
        wait wg
      it "callback is called when expired" $ \(_, client, _, _) -> do
        wg <- newWaitGroup 1
        _ <- request client "foo" [withSubscriptionExpiry 1] (\x -> do
          case x of
            Nothing -> done wg
            Just _  -> error "should not receive message"
         )
        Client.close client
        wait wg
