{-# LANGUAGE OverloadedStrings #-}

module ClientSpec where

import           API
    ( Client (..)
    , MsgView (..)
    , withPayload
    , withQueueGroup
    , withReplyTo
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
import qualified JetStream.API             as JetStream
import           JetStream.Client          (newJetStream)
import           Network.Socket            (Socket, accept, listen)
import qualified Network.Socket
import           Network.Socket.ByteString (recv, sendAll)
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

data CapturedPublish = CapturedPublish
                         { capturedInbox   :: BS.ByteString
                         , capturedSid     :: BS.ByteString
                         , capturedSubject :: BS.ByteString
                         , capturedReplyTo :: BS.ByteString
                         , capturedPayload :: BS.ByteString
                         }
  deriving (Eq, Show)

recvUntil :: Socket -> (BS.ByteString -> Bool) -> IO BS.ByteString
recvUntil sock done =
  go BS.empty
  where
    go acc
      | done acc = pure acc
      | otherwise = do
          chunk <- recv sock 4096
          if BS.null chunk
            then pure acc
            else go (acc <> chunk)

expectClientCommand :: Socket -> BS.ByteString -> IO ()
expectClientCommand sock command = do
  result <- timeout 1000000 $
    recvUntil sock (BS.isInfixOf command)
  case result of
    Nothing ->
      expectationFailure ("client did not send command: " ++ show command)
    Just bytes ->
      unless (BS.isInfixOf command bytes) $
        expectationFailure ("unexpected client bytes: " ++ show bytes)

expectQueuedSub :: Socket -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO ()
expectQueuedSub sock subject queueGroup sid =
  expectClientCommand sock (BS.concat ["SUB ", subject, " ", queueGroup, " ", sid, "\r\n"])

capturePublish :: Socket -> BS.ByteString -> IO CapturedPublish
capturePublish sock expectedSubject = do
  bytes <- recvUntil sock hasExpectedPublish
  case parseCapturedPublish bytes of
    Nothing ->
      expectationFailure ("could not parse publish from bytes: " ++ show bytes) >>
        fail "could not parse publish"
    Just captured -> do
      capturedSubject captured `shouldBe` expectedSubject
      pure captured
  where
    hasExpectedPublish bytes =
      case parseCapturedPublish bytes of
        Nothing ->
          False
        Just captured ->
          capturedSubject captured == expectedSubject

parseCapturedPublish :: BS.ByteString -> Maybe CapturedPublish
parseCapturedPublish bytes = do
  subLine <- findLineWithPrefix "SUB "
  pubLine <- findLineWithPrefix "PUB "
  let subParts = C.words subLine
      pubParts = C.words pubLine
  inbox <- subParts `at` 1
  sid <- subParts `at` 2
  subject' <- pubParts `at` 1
  reply <- pubParts `at` 2
  lenBytes <- pubParts `at` 3
  len <- readMaybeInt lenBytes
  payloadLine <- payloadAfter pubLine
  let payloadBytes = BS.take len payloadLine
  pure CapturedPublish
    { capturedInbox = inbox
    , capturedSid = sid
    , capturedSubject = subject'
    , capturedReplyTo = reply
    , capturedPayload = payloadBytes
    }
  where
    wireLines = C.split '\n' bytes
    stripCR = C.takeWhile (/= '\r')
    normalizedLines = fmap stripCR wireLines
    findLineWithPrefix prefix =
      case filter (BS.isPrefixOf prefix) normalizedLines of
        []       -> Nothing
        (line:_) -> Just line
    payloadAfter pubLine =
      case dropWhile (/= pubLine) normalizedLines of
        (_:payloadLine:_) -> Just payloadLine
        _                 -> Nothing

at :: [a] -> Int -> Maybe a
at values index =
  if index < length values
    then Just (values !! index)
    else Nothing

readMaybeInt :: BS.ByteString -> Maybe Int
readMaybeInt bytes =
  case reads (C.unpack bytes) of
    [(value, "")] -> Just value
    _             -> Nothing

replyToCapturedPublish :: Socket -> CapturedPublish -> BS.ByteString -> IO ()
replyToCapturedPublish sock captured body =
  sendAll sock (msgFrame (capturedInbox captured) (capturedSid captured) body)

replyStatusToCapturedPublish
  :: Socket
  -> CapturedPublish
  -> BS.ByteString
  -> BS.ByteString
  -> IO ()
replyStatusToCapturedPublish sock captured status description =
  sendAll sock $
    hmsgFrame
      (capturedInbox captured)
      (capturedSid captured)
      [ ("Status", status)
      , ("Description", description)
      ]
      ""

protoStreamInfoResponse :: BS.ByteString
protoStreamInfoResponse =
  "{\"config\":{\"name\":\"PROTO_STREAM\",\"subjects\":[\"PROTO.SUBJECT\"],\"retention\":\"limits\",\"storage\":\"memory\",\"discard\":\"new\",\"max_msgs\":1,\"max_bytes\":-1,\"max_age\":0,\"num_replicas\":1,\"allow_direct\":false},\"created\":\"2024-01-01T00:00:00Z\",\"state\":{\"messages\":0,\"bytes\":0,\"first_seq\":0,\"first_ts\":\"2024-01-01T00:00:00Z\",\"last_seq\":0,\"last_ts\":\"2024-01-01T00:00:00Z\",\"consumer_count\":0}}"

protoConsumerInfoResponse :: BS.ByteString
protoConsumerInfoResponse =
  "{\"stream_name\":\"PROTO_STREAM\",\"name\":\"PROTO_CONSUMER\",\"created\":\"2024-01-01T00:00:00Z\",\"config\":{\"deliver_policy\":\"all\",\"ack_policy\":\"explicit\",\"replay_policy\":\"instant\",\"filter_subjects\":[\"PROTO.A\",\"PROTO.B\"]}}"

protoPushConsumerInfoResponse :: BS.ByteString
protoPushConsumerInfoResponse =
  "{\"stream_name\":\"PROTO_STREAM\",\"name\":\"PROTO_PUSH\",\"created\":\"2024-01-01T00:00:00Z\",\"config\":{\"name\":\"PROTO_PUSH\",\"deliver_subject\":\"PROTO.DELIVER\",\"deliver_group\":\"workers\",\"deliver_policy\":\"all\",\"ack_policy\":\"explicit\",\"replay_policy\":\"instant\"}}"

protoOrderedConsumerInfoResponse :: BS.ByteString
protoOrderedConsumerInfoResponse =
  "{\"stream_name\":\"PROTO_STREAM\",\"name\":\"PROTO_ORDERED_1\",\"created\":\"2024-01-01T00:00:00Z\",\"config\":{\"name\":\"PROTO_ORDERED_1\",\"deliver_policy\":\"by_start_sequence\",\"opt_start_seq\":3,\"ack_policy\":\"none\",\"replay_policy\":\"instant\",\"max_deliver\":-1,\"max_waiting\":512,\"inactive_threshold\":300000000000,\"num_replicas\":1,\"mem_storage\":true}}"

protoAccountInfoResponse :: BS.ByteString
protoAccountInfoResponse =
  "{\"memory\":10,\"storage\":20,\"reserved_memory\":0,\"reserved_storage\":0,\"streams\":1,\"consumers\":1,\"limits\":{\"max_memory\":-1,\"max_storage\":-1,\"max_streams\":-1,\"max_consumers\":-1,\"max_ack_pending\":-1,\"memory_max_stream_bytes\":-1,\"storage_max_stream_bytes\":-1,\"max_bytes_required\":false},\"api\":{\"level\":1,\"total\":3,\"errors\":0},\"tiers\":{}}"

protoStreamMessageResponse :: BS.ByteString
protoStreamMessageResponse =
  "{\"message\":{\"subject\":\"PROTO.SUBJECT\",\"seq\":2,\"data\":\"YWRtaW4=\",\"time\":\"2024-01-01T00:00:00Z\"}}"

protoDeleteStreamMessageResponse :: BS.ByteString
protoDeleteStreamMessageResponse =
  "{\"success\":true}"

protoConsumerPauseResponse :: BS.ByteString
protoConsumerPauseResponse =
  "{\"paused\":true,\"pause_until\":\"2024-01-01T00:00:01Z\",\"pause_remaining\":1000000000}"

protoConsumerResetResponse :: BS.ByteString
protoConsumerResetResponse =
  "{\"stream_name\":\"PROTO_STREAM\",\"name\":\"PROTO_CONSUMER\",\"created\":\"2024-01-01T00:00:00Z\",\"config\":{\"deliver_policy\":\"all\",\"ack_policy\":\"explicit\",\"replay_policy\":\"instant\"},\"delivered\":{\"consumer_seq\":1,\"stream_seq\":7},\"ack_floor\":{\"consumer_seq\":0,\"stream_seq\":0},\"num_ack_pending\":0,\"num_redelivered\":0,\"num_waiting\":0,\"num_pending\":3,\"reset_seq\":7}"

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
      it "subscribes with a queue group" $ \(serv, client, _, _) -> do
        sid <- subscribe client "JOBS" [withQueueGroup "WORKERS"] (const (pure ()))
        expectQueuedSub serv "JOBS" "WORKERS" sid
      it "publishes with an explicit reply subject" $ \(serv, client, _, _) -> do
        publish client "REQUESTS" [withPayload "hello", withReplyTo "_INBOX.custom"]
        expectClientCommand serv "PUB REQUESTS _INBOX.custom 5\r\nhello\r\n"
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
    describe "jetstream protocol integration" $ do
      around withClient $ do
        it "sends stream create options as a JetStream API request" $ \(serv, client, _, _) -> do
          let jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.create (JetStream.streams jetStream)
              "PROTO_STREAM"
              ["PROTO.SUBJECT"]
              [ JetStream.withStorage JetStream.MemoryStorage
              , JetStream.withMaxMessages 1
              , JetStream.withDiscard JetStream.DiscardNew
              ]
            putMVar done result
          captured <- capturePublish serv "$JS.API.STREAM.CREATE.PROTO_STREAM"
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"max_msgs\":1"
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"discard\":\"new\""
          replyToCapturedPublish serv captured protoStreamInfoResponse
          result <- timeout 1000000 (takeMVar done)
          case result of
            Nothing ->
              expectationFailure "stream create did not complete"
            Just (Left err) ->
              expectationFailure ("stream create failed: " ++ show err)
            Just (Right info) ->
              JetStream.streamConfigName (JetStream.streamInfoConfig info) `shouldBe` "PROTO_STREAM"

        it "sends only the selected consumer filter representation" $ \(serv, client, _, _) -> do
          let jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.createDurableConsumer (JetStream.consumers jetStream)
              "PROTO_STREAM"
              "PROTO_CONSUMER"
              [ JetStream.withConsumerDeliverPolicy JetStream.DeliverAll
              , JetStream.withConsumerAckPolicy JetStream.AckExplicit
              , JetStream.withConsumerFilter
                  (JetStream.ConsumerFilterSubjects ["PROTO.A", "PROTO.B"])
              ]
            putMVar done result
          captured <- capturePublish serv "$JS.API.CONSUMER.DURABLE.CREATE.PROTO_STREAM.PROTO_CONSUMER"
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"filter_subjects\":[\"PROTO.A\",\"PROTO.B\"]"
          capturedPayload captured `shouldSatisfy` not . BS.isInfixOf "\"filter_subject\":"
          replyToCapturedPublish serv captured protoConsumerInfoResponse
          result <- timeout 1000000 (takeMVar done)
          case result of
            Nothing ->
              expectationFailure "consumer create did not complete"
            Just (Left err) ->
              expectationFailure ("consumer create failed: " ++ show err)
            Just (Right info) ->
              JetStream.consumerInfoName info `shouldBe` "PROTO_CONSUMER"

        it "sends push consumer create requests with a delivery subject" $ \(serv, client, _, _) -> do
          let jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.createPushConsumer
              (JetStream.consumers jetStream)
              "PROTO_STREAM"
              "PROTO_PUSH"
              "PROTO.DELIVER"
              [ JetStream.withConsumerDeliverGroup "workers"
              , JetStream.withConsumerAckPolicy JetStream.AckExplicit
              ]
            putMVar done result
          captured <- capturePublish serv "$JS.API.CONSUMER.CREATE.PROTO_STREAM.PROTO_PUSH"
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"action\":\"create\""
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"deliver_subject\":\"PROTO.DELIVER\""
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"deliver_group\":\"workers\""
          replyToCapturedPublish serv captured protoPushConsumerInfoResponse
          result <- timeout 1000000 (takeMVar done)
          case result of
            Nothing ->
              expectationFailure "push consumer create did not complete"
            Just (Left err) ->
              expectationFailure ("push consumer create failed: " ++ show err)
            Just (Right info) -> do
              JetStream.consumerInfoName info `shouldBe` "PROTO_PUSH"
              JetStream.consumerConfigDeliverSubject (JetStream.consumerInfoConfig info)
                `shouldBe` Just "PROTO.DELIVER"

        it "subscribes to push consumer delivery subjects with a queue group" $ \(serv, client, _, _) -> do
          let jetStream = newJetStream client []
          subscription <- JetStream.consumePush
            (JetStream.messages jetStream)
            "PROTO.DELIVER"
            [JetStream.withPushQueueGroup "workers"]
            (const (pure ()))
          expectClientCommand serv "SUB PROTO.DELIVER workers "
          JetStream.stopPushSubscription subscription
          expectClientCommand serv "UNSUB "

        it "creates ordered consumers with the ordered defaults" $ \(serv, client, _, _) -> do
          let jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.createOrderedConsumer
              (JetStream.messages jetStream)
              "PROTO_STREAM"
              [ JetStream.withOrderedConsumerNamePrefix "PROTO_ORDERED"
              , JetStream.withOrderedConsumerDeliverPolicy (JetStream.DeliverByStartSequence 3)
              ]
            putMVar done result
          captured <- capturePublish serv "$JS.API.CONSUMER.CREATE.PROTO_STREAM.PROTO_ORDERED_1"
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"name\":\"PROTO_ORDERED_1\""
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"deliver_policy\":\"by_start_sequence\""
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"opt_start_seq\":3"
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"ack_policy\":\"none\""
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"max_deliver\":-1"
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"max_waiting\":512"
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"mem_storage\":true"
          replyToCapturedPublish serv captured protoOrderedConsumerInfoResponse
          result <- timeout 1000000 (takeMVar done)
          case result of
            Nothing ->
              expectationFailure "ordered consumer create did not complete"
            Just (Left err) ->
              expectationFailure ("ordered consumer create failed: " ++ show err)
            Just (Right _) ->
              pure ()

        it "sends no-wait pull requests and decodes no-message status" $ \(serv, client, _, _) -> do
          let jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            response <- JetStream.fetch (JetStream.messages jetStream)
              "PROTO_STREAM"
              "PROTO_CONSUMER"
              [ JetStream.withFetchBatch 1
              , JetStream.withFetchWait (JetStream.FetchNoWaitMicros 1000000)
              ]
            putMVar done response
          captured <- capturePublish serv "$JS.API.CONSUMER.MSG.NEXT.PROTO_STREAM.PROTO_CONSUMER"
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"no_wait\":true"
          capturedPayload captured `shouldSatisfy` not . BS.isInfixOf "\"expires\":"
          capturedReplyTo captured `shouldBe` capturedInbox captured
          replyStatusToCapturedPublish serv captured "404" "No Messages"
          result <- timeout 1000000 (takeMVar done)
          case result of
            Nothing ->
              expectationFailure "fetch did not complete"
            Just response -> do
              JetStream.pullResponseMessages response `shouldBe` []
              JetStream.pullResponseStatus response
                `shouldBe` Just (JetStream.PullNoMessages (Just "No Messages"))

        it "sends account info requests" $ \(serv, client, _, _) -> do
          let jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.accountInfo jetStream
            putMVar done result
          captured <- capturePublish serv "$JS.API.INFO"
          capturedPayload captured `shouldBe` ""
          replyToCapturedPublish serv captured protoAccountInfoResponse
          result <- timeout 1000000 (takeMVar done)
          case result of
            Nothing ->
              expectationFailure "account info did not complete"
            Just (Left err) ->
              expectationFailure ("account info failed: " ++ show err)
            Just (Right info) ->
              JetStream.accountTierStreams (JetStream.accountInfoTier info) `shouldBe` 1

        it "sends stream message get and delete requests" $ \(serv, client, _, _) -> do
          let jetStream = newJetStream client []
          fetched <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.getMessage
              (JetStream.streams jetStream)
              "PROTO_STREAM"
              (JetStream.StreamMessageBySequence 2)
            putMVar fetched result
          getRequest <- capturePublish serv "$JS.API.STREAM.MSG.GET.PROTO_STREAM"
          capturedPayload getRequest `shouldSatisfy` BS.isInfixOf "\"seq\":2"
          replyToCapturedPublish serv getRequest protoStreamMessageResponse
          fetchedResult <- timeout 1000000 (takeMVar fetched)
          case fetchedResult of
            Nothing ->
              expectationFailure "stream message get did not complete"
            Just (Left err) ->
              expectationFailure ("stream message get failed: " ++ show err)
            Just (Right message) ->
              JetStream.streamMessagePayload message `shouldBe` Just "admin"

          deleted <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.deleteMessage
              (JetStream.streams jetStream)
              "PROTO_STREAM"
              2
              JetStream.DeleteMessage
            putMVar deleted result
          deleteRequest <- capturePublish serv "$JS.API.STREAM.MSG.DELETE.PROTO_STREAM"
          capturedPayload deleteRequest `shouldSatisfy` BS.isInfixOf "\"seq\":2"
          capturedPayload deleteRequest `shouldSatisfy` BS.isInfixOf "\"no_erase\":true"
          replyToCapturedPublish serv deleteRequest protoDeleteStreamMessageResponse
          deletedResult <- timeout 1000000 (takeMVar deleted)
          case deletedResult of
            Nothing ->
              expectationFailure "stream message delete did not complete"
            Just (Left err) ->
              expectationFailure ("stream message delete failed: " ++ show err)
            Just (Right response) ->
              JetStream.deleteStreamMessageSuccess response `shouldBe` True

        it "sends consumer update actions" $ \(serv, client, _, _) -> do
          let jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.updateDurableConsumer
              (JetStream.consumers jetStream)
              "PROTO_STREAM"
              "PROTO_CONSUMER"
              [ JetStream.withConsumerDescription "updated"
              , JetStream.withConsumerAckPolicy JetStream.AckExplicit
              ]
            putMVar done result
          captured <- capturePublish serv "$JS.API.CONSUMER.CREATE.PROTO_STREAM.PROTO_CONSUMER"
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"action\":\"update\""
          capturedPayload captured `shouldSatisfy` BS.isInfixOf "\"description\":\"updated\""
          replyToCapturedPublish serv captured protoConsumerInfoResponse
          result <- timeout 1000000 (takeMVar done)
          case result of
            Nothing ->
              expectationFailure "consumer update did not complete"
            Just (Left err) ->
              expectationFailure ("consumer update failed: " ++ show err)
            Just (Right info) ->
              JetStream.consumerInfoName info `shouldBe` "PROTO_CONSUMER"

        it "sends consumer pause and reset requests" $ \(serv, client, _, _) -> do
          let jetStream = newJetStream client []
              pauseUntil = read "2024-01-01 00:00:01 UTC"
          paused <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.pauseConsumer
              (JetStream.consumers jetStream)
              "PROTO_STREAM"
              "PROTO_CONSUMER"
              pauseUntil
            putMVar paused result
          pauseRequest <- capturePublish serv "$JS.API.CONSUMER.PAUSE.PROTO_STREAM.PROTO_CONSUMER"
          capturedPayload pauseRequest `shouldSatisfy` BS.isInfixOf "\"pause_until\":\"2024-01-01T00:00:01Z\""
          replyToCapturedPublish serv pauseRequest protoConsumerPauseResponse
          pausedResult <- timeout 1000000 (takeMVar paused)
          case pausedResult of
            Nothing ->
              expectationFailure "consumer pause did not complete"
            Just (Left err) ->
              expectationFailure ("consumer pause failed: " ++ show err)
            Just (Right response) ->
              JetStream.consumerPausePaused response `shouldBe` True

          reset <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.resetConsumer
              (JetStream.consumers jetStream)
              "PROTO_STREAM"
              "PROTO_CONSUMER"
              [JetStream.withConsumerResetSequence 7]
            putMVar reset result
          resetRequest <- capturePublish serv "$JS.API.CONSUMER.RESET.PROTO_STREAM.PROTO_CONSUMER"
          capturedPayload resetRequest `shouldSatisfy` BS.isInfixOf "\"seq\":7"
          replyToCapturedPublish serv resetRequest protoConsumerResetResponse
          resetResult <- timeout 1000000 (takeMVar reset)
          case resetResult of
            Nothing ->
              expectationFailure "consumer reset did not complete"
            Just (Left err) ->
              expectationFailure ("consumer reset failed: " ++ show err)
            Just (Right response) ->
              JetStream.consumerResetResponseSequence response `shouldBe` 7
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
    it "resubscribes with a queue group after reconnect" $ do
      (p, sock) <- openFreePort
      listen sock 2
      clientVar <- newEmptyMVar
      void . forkIO $ do
        let configOptions =
              [ withMinimumLogLevel Debug
              , withConnectionAttempts 2
              , withConnectName "test-client"
              ]
        c <- newClient [("127.0.0.1", p)] configOptions
        putMVar clientVar c
      (firstConn, _) <- accept sock
      sendAll firstConn defaultINFO
      clientResult <- timeout 1000000 (takeMVar clientVar)
      case clientResult of
        Nothing -> expectationFailure "client did not connect"
        Just client -> do
          sid <- subscribe client "JOBS" [withQueueGroup "WORKERS"] (const (pure ()))
          expectQueuedSub firstConn "JOBS" "WORKERS" sid
          Network.Socket.close firstConn
          (secondConn, _) <- accept sock
          sendAll secondConn defaultINFO
          expectQueuedSub secondConn "JOBS" "WORKERS" sid
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
