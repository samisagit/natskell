{-# LANGUAGE OverloadedStrings #-}

module ClientSpec where

import           API
    ( Client (..)
    , MsgView (..)
    , withSubscriptionExpiry
    )
import           Client
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C
import           Data.Foldable             (for_)
import           Data.IORef
    ( atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import           Data.Word8
import           Network.Socket            (Socket, accept, listen)
import qualified Network.Socket
import           Network.Socket.ByteString (sendAll)
import           Network.Socket.Free
import           System.Timeout            (timeout)
import           Test.Hspec
import           WaitGroup

defaultINFO = "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3}\r\n"

tooLongMSG = "MSG a b 5000\r\n" <> BS.replicate 5000 _x <> "\r\n"

headerBlock :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
headerBlock hs =
  BS.concat ("NATS/1.0\r\n" : foldr appendHeader ["\r\n"] hs)
  where
    appendHeader (key, value) acc = key : ":" : value : "\r\n" : acc

msgFrame :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
msgFrame subject sid payload =
  BS.concat
    [ "MSG "
    , subject
    , " "
    , sid
    , " "
    , C.pack (show (BS.length payload))
    , "\r\n"
    , payload
    , "\r\n"
    ]

hmsgFrame :: BS.ByteString -> BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString -> BS.ByteString
hmsgFrame subject sid headers payload =
  let headerBytes = headerBlock headers
      headerLen = BS.length headerBytes
      totalLen = headerLen + BS.length payload
  in BS.concat
    [ "HMSG "
    , subject
    , " "
    , sid
    , " "
    , C.pack (show headerLen)
    , " "
    , C.pack (show totalLen)
    , "\r\n"
    , headerBytes
    , payload
    , "\r\n"
    ]

startClientWith extraOptions = do
  (p, sock) <- openFreePort
  listen sock 1
  tva <- newEmptyTMVarIO
  void . forkIO $ do
    (serv, _) <- accept sock
    atomically $ putTMVar tva serv
  exited <- newEmptyTMVarIO
  tvb <- newEmptyTMVarIO
  void . forkIO $ do
    let configOptions =
          extraOptions
            ++ [ withExitAction (atomically . putTMVar exited)
               , withMinimumLogLevel Debug
               , withConnectionAttempts 1
               , withConnectName "test-client"
               ]
    c <- newClient [("127.0.0.1", p)] configOptions
    atomically $ putTMVar tvb c

  s <- atomically $ takeTMVar tva
  sendAll s defaultINFO
  c <- atomically $ takeTMVar tvb
  return (s, c, sock, exited)

startClient :: IO (Socket, Client, Socket, TMVar ClientExitReason)
startClient =
  startClientWith []

stopClient :: (Socket, Client, Socket, TMVar ClientExitReason) -> IO ()
stopClient (s, c, sock, _) = do
  close c
  Network.Socket.close sock
  Network.Socket.close s

stopClientSafely :: (Socket, Client, Socket, TMVar ClientExitReason) -> IO ()
stopClientSafely (s, c, sock, _) = do
  void (try (close c) :: IO (Either SomeException ()))
  void (try (Network.Socket.close sock) :: IO (Either SomeException ()))
  void (try (Network.Socket.close s) :: IO (Either SomeException ()))

withClient :: ((Socket, Client, Socket, TMVar ClientExitReason) -> IO()) -> IO ()
withClient action = do
  bracket startClient stopClient action

withClientWith configOptions =
  bracket (startClientWith configOptions) stopClient

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
        close client
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
      it "MSG subject split over frames is joined" $ \(serv, client, _, _) -> do
        msgVar <- newEmptyMVar
        let topic = "SOAK.SUBJECT"
            payloadValue = "HELLO"
        sid <- subscribe client topic [] (putMVar msgVar)
        sendAll serv "MSG SOAK."
        threadDelay 100000
        let headerTail =
              "SUBJECT "
                <> sid
                <> " "
                <> C.pack (show (BS.length payloadValue))
                <> "\r\n"
            chunk = headerTail <> payloadValue <> "\r\n"
        sendAll serv chunk
        result <- timeout 1000000 (takeMVar msgVar)
        case result of
          Nothing -> expectationFailure "message not received"
          Just Nothing -> expectationFailure "received empty message"
          Just (Just msg) -> do
            subject msg `shouldBe` topic
            payload msg `shouldBe` Just payloadValue
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
        close client
        wait wg
    it "manual unsubscribe does not block close for tracked expiries" $ do
      bracket startClient stopClientSafely $ \(_, client, _, _) -> do
        sid <- request client "foo" [withSubscriptionExpiry 30] (const (pure ()))
        unsubscribe client sid
        result <- timeout 1000000 (close client)
        case result of
          Nothing -> expectationFailure "close blocked after unsubscribe"
          Just () -> pure ()
    it "early replies do not block close for tracked expiries" $ do
      bracket startClient stopClientSafely $ \(serv, client, _, _) -> do
        replyBox <- newEmptyMVar
        sid <- request client "foo" [withSubscriptionExpiry 30] (putMVar replyBox)
        sendAll serv (msgFrame "foo" sid "bar")
        reply <- timeout 1000000 (takeMVar replyBox)
        case reply of
          Nothing         -> expectationFailure "reply callback did not run"
          Just Nothing    -> expectationFailure "expected a reply message"
          Just (Just msg) -> payload msg `shouldBe` Just "bar"
        result <- timeout 1000000 (close client)
        case result of
          Nothing -> expectationFailure "close blocked after reply"
          Just () -> pure ()
    it "retries when the server disconnects before INFO" $ do
      (p, sock) <- openFreePort
      listen sock 2
      serverConn <- newEmptyTMVarIO
      clientVar <- newEmptyMVar
      void . forkIO $ do
        (firstConn, _) <- accept sock
        Network.Socket.close firstConn
        (secondConn, _) <- accept sock
        sendAll secondConn defaultINFO
        atomically $ putTMVar serverConn secondConn
      void . forkIO $ do
        let configOptions =
              [ withMinimumLogLevel Debug
              , withConnectionAttempts 2
              , withConnectName "test-client"
              ]
        c <- newClient [("127.0.0.1", p)] configOptions
        putMVar clientVar c
      clientResult <- timeout 1000000 (takeMVar clientVar)
      case clientResult of
        Nothing -> expectationFailure "client did not recover after disconnect before INFO"
        Just client -> do
          secondConn <- atomically $ takeTMVar serverConn
          wg <- newWaitGroup 1
          ping client $ done wg
          sendAll secondConn "PONG\r\n"
          wait wg
          close client
          Network.Socket.close secondConn
      Network.Socket.close sock
    around (withClientWith [withBufferLimit (64 * 1024), withCallbackConcurrency 4]) $ do
      it "soak: parses a large buffer of MSG and HMSG frames" $ \(serv, client, _, _) -> do
        let subject = "SOAK.SUBJECT"
            smallPayloadSize = 512
            largePayloadSize = 16 * 1024
            smallCount = 100000
            largeCount = 2000
            headerValue = BS.replicate 128 _x
            headerPairs =
              [ ("X-Header-1", headerValue)
              , ("X-Header-2", headerValue)
              , ("X-Header-3", headerValue)
              , ("X-Header-4", headerValue)
              ]
        let smallPayload = BS.replicate smallPayloadSize _x
        let largePayload = BS.replicate largePayloadSize _x
        let expectedTotal = smallCount + largeCount
        let timeoutMicros = 120 * 1000000
        counter <- newIORef 0
        headerCounter <- newIORef 0
        headerChecked <- newIORef False
        errorRef <- newIORef Nothing
        done <- newEmptyMVar
        let recordError err =
              atomicModifyIORef' errorRef $ \current ->
                case current of
                  Nothing -> (Just err, ())
                  Just _  -> (current, ())
        let handleMsg msg = do
              let payloadLen = maybe 0 BS.length (payload msg)
              case headers msg of
                Just hs -> do
                  atomicModifyIORef' headerCounter $ \count -> (count + 1, ())
                  when (payloadLen /= largePayloadSize) $
                    recordError "unexpected headers on small payload"
                  checked <- readIORef headerChecked
                  unless checked $ do
                    when (hs /= headerPairs) $
                      recordError "unexpected header contents"
                    writeIORef headerChecked True
                Nothing ->
                  when (payloadLen == largePayloadSize) $
                    recordError "missing headers on large payload"
              count <- atomicModifyIORef' counter $ \count' ->
                let nextCount = count' + 1
                in (nextCount, nextCount)
              when (count == expectedTotal) $
                void (tryPutMVar done ())
        sid <- subscribe client subject [] (maybe (pure ()) handleMsg)
        let msg = msgFrame subject sid smallPayload
        let hmsg = hmsgFrame subject sid headerPairs largePayload
        let buffer = BS.concat (replicate smallCount msg ++ replicate largeCount hmsg)
        sendAll serv buffer
        result <- timeout timeoutMicros (takeMVar done)
        case result of
          Nothing -> expectationFailure "soak buffer timed out"
          Just () -> pure ()
        errors <- readIORef errorRef
        for_ errors expectationFailure
        headersSeen <- readIORef headerCounter
        headersSeen `shouldBe` largeCount
