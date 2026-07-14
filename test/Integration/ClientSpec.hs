{-# LANGUAGE OverloadedStrings #-}

module ClientSpec where

import           API
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
import           JetStream.Client
    ( newJetStream
    , withRequestTimeoutMicros
    )
import           Network.Socket            (Socket, accept, listen)
import qualified Network.Socket
import           Network.Socket.ByteString (recv, sendAll)
import           Network.Socket.Free
import           System.Timeout            (timeout)
import           Test.Hspec
import           WaitGroup

defaultINFO = "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3}\r\n"

newClientOrFail servers options = do
  result <- newClient servers options
  case result of
    Left err     -> fail ("client failed to connect: " ++ show err)
    Right client -> pure client

pingWithCallback :: Client -> IO () -> IO ()
pingWithCallback client action =
  void . forkIO $ void (ping client []) >> action

isValidationFailure :: Either NatsError a -> Bool
isValidationFailure (Left (NatsValidationError _)) = True
isValidationFailure _                              = False

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

data CapturedSubscription = CapturedSubscription
                              { capturedSubSubject    :: BS.ByteString
                              , capturedSubQueueGroup :: Maybe BS.ByteString
                              , capturedSubSid        :: BS.ByteString
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

expectClientBytes :: Socket -> (BS.ByteString -> Bool) -> String -> IO BS.ByteString
expectClientBytes sock done failureMessage = do
  result <- timeout 10000000 $
    recvUntil sock done
  case result of
    Nothing ->
      expectationFailure failureMessage >>
        fail failureMessage
    Just bytes ->
      if done bytes
        then pure bytes
        else
          expectationFailure ("unexpected client bytes: " ++ show bytes) >>
            fail failureMessage

expectNoClientBytesBefore
  :: Socket
  -> (BS.ByteString -> Bool)
  -> BS.ByteString
  -> String
  -> IO ()
expectNoClientBytesBefore sock unwanted barrier failureMessage = do
  bytes <-
    expectClientBytes
      sock
      (BS.isInfixOf barrier)
      ("client did not send barrier command: " ++ show barrier)
  when (unwanted bytes) $
    expectationFailure (failureMessage ++ ": " ++ show bytes)

expectClientCommand :: Socket -> BS.ByteString -> IO ()
expectClientCommand sock command = do
  result <- timeout 10000000 $
    recvUntil sock (BS.isInfixOf command)
  case result of
    Nothing ->
      expectationFailure ("client did not send command: " ++ show command)
    Just bytes ->
      unless (BS.isInfixOf command bytes) $
        expectationFailure ("unexpected client bytes: " ++ show bytes)

expectMVar :: String -> MVar a -> IO a
expectMVar failureMessage var = do
  result <- timeout 10000000 (takeMVar var)
  case result of
    Nothing ->
      expectationFailure failureMessage >>
        fail failureMessage
    Just value ->
      pure value

completeHandshake :: Socket -> IO BS.ByteString
completeHandshake sock = do
  sendAll sock defaultINFO
  bytes <-
    expectClientBytes
      sock
      (\sent -> BS.isInfixOf "CONNECT " sent && BS.isInfixOf "PING\r\n" sent)
      "client did not complete the initial handshake"
  sendAll sock "PONG\r\n"
  pure bytes

expectQueuedSub :: Socket -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO ()
expectQueuedSub sock subject queueGroup sid =
  expectClientCommand sock (BS.concat ["SUB ", subject, " ", queueGroup, " ", sid, "\r\n"])

captureSubscription :: Socket -> BS.ByteString -> IO CapturedSubscription
captureSubscription sock expectedSubject = do
  bytes <- recvUntil sock hasExpectedSubscription
  case parseCapturedSubscription expectedSubject bytes of
    Nothing ->
      expectationFailure ("could not parse subscription from bytes: " ++ show bytes) >>
        fail "could not parse subscription"
    Just captured ->
      pure captured
  where
    hasExpectedSubscription bytes =
      case parseCapturedSubscription expectedSubject bytes of
        Nothing -> False
        Just _  -> True

parseCapturedSubscription :: BS.ByteString -> BS.ByteString -> Maybe CapturedSubscription
parseCapturedSubscription expectedSubject bytes =
  findSubscription normalizedLines
  where
    wireLines = C.split '\n' bytes
    stripCR = C.takeWhile (/= '\r')
    normalizedLines = fmap stripCR wireLines
    findSubscription [] = Nothing
    findSubscription (line:rest) =
      case C.words line of
        [prefix, subject', sid]
          | prefix == "SUB" && subject' == expectedSubject ->
              Just (CapturedSubscription subject' Nothing sid)
        [prefix, subject', queueGroup, sid]
          | prefix == "SUB" && subject' == expectedSubject ->
              Just (CapturedSubscription subject' (Just queueGroup) sid)
        _ ->
          findSubscription rest

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
  pubLine <- findPublishLine
  let subParts = C.words subLine
      pubParts = C.words pubLine
  inbox <- subParts `at` 1
  sid <- subParts `at` 2
  (subject', reply, headerLength, totalLength) <- publishLengths pubParts
  frameBody <- frameBodyAfter pubLine
  let payloadBytes =
        BS.take (totalLength - headerLength) (BS.drop headerLength frameBody)
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
    findPublishLine =
      case filter isPublishLine normalizedLines of
        []       -> Nothing
        (line:_) -> Just line
    isPublishLine line =
      "PUB " `BS.isPrefixOf` line || "HPUB " `BS.isPrefixOf` line
    publishLengths parts =
      case parts of
        ["PUB", subject', reply, lengthBytes] -> do
          totalLength <- readMaybeInt lengthBytes
          pure (subject', reply, 0, totalLength)
        ["HPUB", subject', reply, headerLengthBytes, totalLengthBytes] -> do
          headerLength <- readMaybeInt headerLengthBytes
          totalLength <- readMaybeInt totalLengthBytes
          pure (subject', reply, headerLength, totalLength)
        _ ->
          Nothing
    frameBodyAfter line =
      let marker = line <> "\r\n"
          (_, suffix) = C.breakSubstring marker bytes
      in if BS.null suffix
           then Nothing
           else Just (BS.drop (BS.length marker) suffix)

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

appearsBefore :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool
appearsBefore first second bytes =
  case (C.breakSubstring first bytes, C.breakSubstring second bytes) of
    ((firstPrefix, firstMatch), (secondPrefix, secondMatch)) ->
      not (BS.null firstMatch)
        && not (BS.null secondMatch)
        && BS.length firstPrefix < BS.length secondPrefix

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

protoOrderedConsumerInfoResponseFor :: BS.ByteString -> BS.ByteString
protoOrderedConsumerInfoResponseFor consumer =
  BS.concat
    [ "{\"stream_name\":\"PROTO_STREAM\",\"name\":\""
    , consumer
    , "\",\"created\":\"2024-01-01T00:00:00Z\",\"config\":{\"name\":\""
    , consumer
    , "\",\"deliver_policy\":\"all\",\"ack_policy\":\"none\",\"replay_policy\":\"instant\",\"max_deliver\":-1,\"max_waiting\":512,\"inactive_threshold\":300000000000,\"num_replicas\":1,\"mem_storage\":true}}"
    ]

protoAccountInfoResponse :: BS.ByteString
protoAccountInfoResponse =
  "{\"memory\":10,\"storage\":20,\"reserved_memory\":0,\"reserved_storage\":0,\"streams\":1,\"consumers\":1,\"limits\":{\"max_memory\":-1,\"max_storage\":-1,\"max_streams\":-1,\"max_consumers\":-1,\"max_ack_pending\":-1,\"memory_max_stream_bytes\":-1,\"storage_max_stream_bytes\":-1,\"max_bytes_required\":false},\"api\":{\"level\":1,\"total\":3,\"errors\":0},\"tiers\":{}}"

protoStreamMessageResponse :: BS.ByteString
protoStreamMessageResponse =
  "{\"message\":{\"subject\":\"PROTO.SUBJECT\",\"seq\":2,\"data\":\"YWRtaW4=\",\"time\":\"2024-01-01T00:00:00Z\"}}"

protoDeleteStreamMessageResponse :: BS.ByteString
protoDeleteStreamMessageResponse =
  "{\"success\":true}"

protoDeleteConsumerResponse :: BS.ByteString
protoDeleteConsumerResponse =
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
    c <- newClientOrFail [("127.0.0.1", p)] configOptions
    atomically $ putTMVar tvb c

  s <- atomically $ takeTMVar tva
  void (completeHandshake s)
  c <- atomically $ takeTMVar tvb
  return (s, c, sock, exited)

startClient :: IO (Socket, Client, Socket, TMVar ClientExitReason)
startClient =
  startClientWith []

stopClient :: (Socket, Client, Socket, TMVar ClientExitReason) -> IO ()
stopClient (s, c, sock, _) = do
  close c []
  Network.Socket.close sock
  Network.Socket.close s

stopClientSafely :: (Socket, Client, Socket, TMVar ClientExitReason) -> IO ()
stopClientSafely (s, c, sock, _) = do
  void (try (close c []) :: IO (Either SomeException ()))
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
      it "PING resolves after PONG" $ \(serv, client, _, _) -> do
        resultVar <- newEmptyMVar
        void . forkIO $ ping client [] >>= putMVar resultVar
        expectClientCommand serv "PING\r\n"
        sendAll serv "PONG\r\n"
        expectMVar "PING did not resolve after PONG" resultVar
          `shouldReturn` Right ()
      it "flush resolves after PONG" $ \(serv, client, _, _) -> do
        resultVar <- newEmptyMVar
        void . forkIO $ flush client [] >>= putMVar resultVar
        expectClientCommand serv "PING\r\n"
        sendAll serv "PONG\r\n"
        expectMVar "flush did not resolve after PONG" resultVar
          `shouldReturn` Right ()
      it "subscribes with a queue group" $ \(serv, client, _, _) -> do
        Right subscription <- subscribe client "JOBS" [withQueueGroup "WORKERS"] (const (pure ()))
        expectQueuedSub serv "JOBS" "WORKERS" (subscriptionSid subscription)
      it "publishes with an explicit reply subject" $ \(serv, client, _, _) -> do
        publish client "REQUESTS" "hello" [withReplyTo "_INBOX.custom"] `shouldReturn` Right ()
        expectClientCommand serv "PUB REQUESTS _INBOX.custom 5\r\nhello\r\n"
      it "reports user initiated close" $ \(_, client, _, exited) -> do
        close client []
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
        pingWithCallback client $ done wg
        expectClientCommand serv "PING\r\n"
        sendAll serv "PONG\r\n"
        wait wg
      it "garbled prefix bytes are ignored" $ \(serv, client, _, _) -> do
        wg <- newWaitGroup 1
        pingWithCallback client $ done wg
        expectClientCommand serv "PING\r\n"
        sendAll serv "ldkfjajhfklsjhlkajf;alwfPONG\r\n"
        wait wg
      it "garbled suffix bytes remove partial prefix" $ \(serv, client, _, _) -> do
        wg <- newWaitGroup 1
        pingWithCallback client $ done wg
        expectClientCommand serv "PING\r\n"
        sendAll serv "MSGX"
        sendAll serv "PONG\r\n"
        wait wg
      it "messages split over frames are joined" $ \(serv, client, _, _) -> do
        wg <- newWaitGroup 1
        pingWithCallback client $ done wg
        expectClientCommand serv "PING\r\n"
        sendAll serv "PON"
        sendAll serv "G\r\n"
        wait wg
      it "MSG subject split over frames is joined" $ \(serv, client, _, _) -> do
        msgVar <- newEmptyMVar
        let topic = "SOAK.SUBJECT"
            payloadValue = "HELLO"
        Right subscription <- subscribe client topic [] (putMVar msgVar)
        let sid = subscriptionSid subscription
        sendAll serv "MSG SOAK."
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
          Just msg -> do
            subject msg `shouldBe` topic
            payload msg `shouldBe` payloadValue
      it "exits when server goes away" $ \(serv, _, _, exited) -> do
        Network.Socket.close serv
        result <- atomically $ readTMVar exited
        case result of
          ExitRetriesExhausted _ -> return ()
          other                  -> expectationFailure $ "Unexpected exit reason: " ++ show other
      it "processes messages larger than the old parser buffer" $ \(serv, client, _, _) -> do
        wg <- newWaitGroup 1
        pingWithCallback client $ done wg
        expectClientCommand serv "PING\r\n"
        sendAll serv tooLongMSG
        sendAll serv "PONG\r\n"
        wait wg
      it "enforces sub-second request timeouts" $ \(_, client, _, _) -> do
        result <- timeout 500000 $
          request client "foo" "" [withRequestTimeout 0.05]
        result `shouldBe` Just (Left NatsRequestTimedOut)
      it "timed out requests do not block close" $ \(_, client, _, _) -> do
        request client "foo" "" [withRequestTimeout 0.01]
          `shouldReturn` Left NatsRequestTimedOut
        timeout 500000 (close client []) `shouldReturn` Just ()

    describe "core NATS fake-server coverage" $ do
      around withClient $ do
        it "ignores duplicate request replies after the first message" $ \(serv, client, _, _) -> do
          replies <- newIORef ([] :: [Message])
          Right subscription <- subscribeOnce client "SERVICE.ONCE" [] $ \reply -> do
            atomicModifyIORef' replies $ \seen -> (seen ++ [reply], ())
          let sid = subscriptionSid subscription
          expectClientCommand serv (BS.concat ["SUB SERVICE.ONCE ", sid, "\r\n"])
          callbacksDrained <- newEmptyMVar
          Right barrier <- subscribe client "CALLBACK.BARRIER" [] $ \_ ->
            putMVar callbacksDrained ()
          let barrierSid = subscriptionSid barrier
          expectClientCommand serv (BS.concat ["SUB CALLBACK.BARRIER ", barrierSid, "\r\n"])
          sendAll serv $
            BS.concat
              [ msgFrame "SERVICE.ONCE" sid "first"
              , msgFrame "SERVICE.ONCE" sid "second"
              , msgFrame "CALLBACK.BARRIER" barrierSid "done"
              ]
          expectMVar "callback barrier did not run" callbacksDrained
          seen <- readIORef replies
          case seen of
            [msg] ->
              payload msg `shouldBe` "first"
            other ->
              expectationFailure ("unexpected request replies: " ++ show other)

        it "handles many simultaneous request replies" $ \(serv, client, _, _) -> do
          requests <- forM [1 .. 32 :: Int] $ \index -> do
            replyBox <- newEmptyMVar
            let requestSubject = C.pack ("SERVICE.MANY." ++ show index)
            Right subscription <- subscribeOnce client requestSubject [] (putMVar replyBox)
            let sid = subscriptionSid subscription
            pure (index, requestSubject, sid, replyBox)
          for_ requests $ \(index, requestSubject, sid, _) ->
            sendAll serv $
              msgFrame requestSubject sid (C.pack ("reply-" ++ show index))
          for_ requests $ \(index, _, _, replyBox) -> do
            reply <- expectMVar ("request reply " ++ show index ++ " did not arrive") replyBox
            payload reply `shouldBe` C.pack ("reply-" ++ show index)

        it "keeps an active request alive after garbled inbound bytes" $ \(serv, client, _, _) -> do
          replyBox <- newEmptyMVar
          Right subscription <- subscribeOnce client "SERVICE.GARBLED" [] (putMVar replyBox)
          let sid = subscriptionSid subscription
          sendAll serv $
            BS.concat
              [ "bad-prefix"
              , msgFrame "SERVICE.GARBLED" sid "ok"
              ]
          reply <- expectMVar "request reply after garbled bytes did not arrive" replyBox
          payload reply `shouldBe` "ok"

        it "round trips headers across request publish and reply" $ \(serv, client, _, _) -> do
          replyBox <- newEmptyMVar
          void . forkIO $
            request client
              "HEADERS.SERVICE"
              "ping"
              [withRequestHeaders [("X-Request", "one")]]
              >>= putMVar replyBox
          captured <- capturePublish serv "HEADERS.SERVICE"
          sendAll serv $
            hmsgFrame
              (capturedInbox captured)
              (capturedSid captured)
              [("X-Reply", "ok")]
              "pong"
          reply <- expectMVar "header reply did not arrive" replyBox
          case reply of
            Left err ->
              expectationFailure ("header request failed: " ++ show err)
            Right msg -> do
              subject msg `shouldBe` capturedInbox captured
              payload msg `shouldBe` "pong"
              headers msg `shouldBe` Just [("X-Reply", "ok")]

        it "flush sends queued publishes before its PING" $ \(serv, client, _, _) -> do
          doneVar <- newEmptyMVar
          let publishFrame = "PUB ORDER.FLUSH 3\r\none\r\n"
          publish client "ORDER.FLUSH" "one" [] `shouldReturn` Right ()
          void . forkIO $ do
            flush client []
            putMVar doneVar ()
          bytes <- expectClientBytes
            serv
            (\sent ->
              BS.isInfixOf publishFrame sent
                && BS.isInfixOf "PING\r\n" sent)
            "client did not send publish and flush PING"
          unless (appearsBefore publishFrame "PING\r\n" bytes) $
            expectationFailure ("flush PING was sent before queued publish: " ++ show bytes)
          sendAll serv "PONG\r\n"
          expectMVar "flush did not complete after PONG" doneVar

        it "exits when the server disconnects while a subscription is active" $ \(serv, client, _, exited) -> do
          Right subscription <- subscribe client "ACTIVE.DISCONNECT" [] (const (pure ()))
          let sid = subscriptionSid subscription
          expectClientCommand serv (BS.concat ["SUB ACTIVE.DISCONNECT ", sid, "\r\n"])
          Network.Socket.close serv
          result <- atomically $ readTMVar exited
          case result of
            ExitRetriesExhausted _ ->
              pure ()
            other ->
              expectationFailure ("Unexpected exit reason: " ++ show other)

        it "does not write publishes above negotiated max_payload" $ \(serv, client, _, _) -> do
          publish client "BOUNDARY.TOO_BIG" (BS.replicate 2048 _x) []
            `shouldReturn` Left (NatsPayloadTooLarge 2048 1024)
          wg <- newWaitGroup 1
          pingWithCallback client (done wg)
          expectNoClientBytesBefore
            serv
            (BS.isInfixOf "PUB BOUNDARY.TOO_BIG")
            "PING\r\n"
            "oversized publish reached server"
          sendAll serv "PONG\r\n"
          wait wg

        it "does not write invalid publish commands" $ \(serv, client, _, _) -> do
          publish client "" "x" [] >>= (`shouldSatisfy` isValidationFailure)
          pingBox <- newEmptyMVar
          pingWithCallback client (putMVar pingBox ())
          expectNoClientBytesBefore
            serv
            (\sent -> BS.isInfixOf "PUB " sent || BS.isInfixOf "HPUB " sent)
            "PING\r\n"
            "invalid publish reached server"
          sendAll serv "PONG\r\n"
          expectMVar "PING failed after an invalid publish" pingBox

      around (withClientWith [withMessageLimit 4]) $ do
        it "does not write publishes above the client message limit" $ \(serv, client, _, _) -> do
          publish client "BOUNDARY.CLIENT" "12345" []
            `shouldReturn` Left (NatsPayloadTooLarge 5 4)
          pingBox <- newEmptyMVar
          pingWithCallback client (putMVar pingBox ())
          expectNoClientBytesBefore
            serv
            (BS.isInfixOf "PUB BOUNDARY.CLIENT")
            "PING\r\n"
            "publish above the client limit reached server"
          sendAll serv "PONG\r\n"
          expectMVar "PING failed after a rejected publish" pingBox

        it "accepts a publish exactly at the client message limit" $ \(serv, client, _, _) -> do
          publish client "BOUNDARY.EXACT" "1234" [] `shouldReturn` Right ()
          expectClientCommand serv "PUB BOUNDARY.EXACT 4\r\n1234\r\n"

        it "counts headers toward the client message limit" $ \(serv, client, _, _) -> do
          let messageHeaders = [("X", "Y")]
              actualSize = BS.length (headerBlock messageHeaders)
          publish client "BOUNDARY.HEADERS" "" [withHeaders messageHeaders]
            `shouldReturn` Left (NatsPayloadTooLarge actualSize 4)
          pingBox <- newEmptyMVar
          pingWithCallback client (putMVar pingBox ())
          expectNoClientBytesBefore
            serv
            (BS.isInfixOf "HPUB BOUNDARY.HEADERS")
            "PING\r\n"
            "publish whose headers exceed the client limit reached server"
          sendAll serv "PONG\r\n"
          expectMVar "PING failed after a rejected header publish" pingBox

        it "terminates the connection when an inbound message exceeds the client limit" $ \(serv, client, _, exited) -> do
          Right subscription <- subscribe client "BOUNDARY.INBOUND" [] (const (pure ()))
          let sid = subscriptionSid subscription
          expectClientCommand serv (BS.concat ["SUB BOUNDARY.INBOUND ", sid, "\r\n"])
          sendAll serv (msgFrame "BOUNDARY.INBOUND" sid "12345")
          atomically (readTMVar exited)
            `shouldReturn` ExitInboundMessageTooLarge 5 4

      around (withClientWith [withPendingDeliveryLimits 1 1024]) $ do
        it "fails a request promptly when the global callback backlog is full" $ \(serv, client, _, _) -> do
          callbackStarted <- newEmptyMVar
          releaseCallback <- newEmptyMVar
          Right subscription <- subscribe client "SLOW.CALLBACK" [] $ \_ -> do
            putMVar callbackStarted ()
            takeMVar releaseCallback
          let sid = subscriptionSid subscription
          expectClientCommand serv (BS.concat ["SUB SLOW.CALLBACK ", sid, "\r\n"])
          sendAll serv (msgFrame "SLOW.CALLBACK" sid "busy")
          expectMVar "blocking callback did not start" callbackStarted

          replyBox <- newEmptyMVar
          void . forkIO $
            request client "SERVICE.OVERLOADED" "request" [withRequestTimeout 2]
              >>= putMVar replyBox
          captured <- capturePublish serv "SERVICE.OVERLOADED"
          sendAll serv $
            msgFrame (capturedInbox captured) (capturedSid captured) "reply"
          expectMVar "overloaded request did not fail promptly" replyBox
            `shouldReturn` Left NatsSlowConsumer

          sendAll serv "PING\r\n"
          expectClientCommand serv "PONG\r\n"
          putMVar releaseCallback ()

      it "cleans up no responders replies for expiring requests" $ do
        bracket startClient stopClientSafely $ \(serv, client, _, _) -> do
          replyBox <- newEmptyMVar
          void . forkIO $
            request client "SERVICE.NO_RESPONDERS" "" [withRequestTimeout 2]
              >>= putMVar replyBox
          captured <- capturePublish serv "SERVICE.NO_RESPONDERS"
          sendAll serv $
            hmsgFrame
              (capturedInbox captured)
              (capturedSid captured)
              [ ("Status", "503")
              , ("Description", "No Responders")
              ]
              ""
          reply <- expectMVar "no responders request did not return" replyBox
          reply `shouldBe` Left NatsNoResponders
          result <- timeout 500000 (close client [])
          case result of
            Nothing ->
              expectationFailure "close blocked after no responders reply"
            Just () ->
              pure ()

      it "does not resubscribe in-flight reply inboxes after reconnect" $ do
        (p, sock) <- openFreePort
        listen sock 2
        clientVar <- newEmptyMVar
        void . forkIO $ do
          let configOptions =
                [ withMinimumLogLevel Debug
                , withConnectionAttempts 2
                , withConnectName "test-client"
                ]
          c <- newClientOrFail [("127.0.0.1", p)] configOptions
          putMVar clientVar c
        (firstConn, _) <- accept sock
        void (completeHandshake firstConn)
        clientResult <- timeout 1000000 (takeMVar clientVar)
        case clientResult of
          Nothing ->
            expectationFailure "client did not connect"
          Just client -> do
            void . forkIO $
              void (request client "SERVICE.RECONNECT" "ping" [withRequestTimeout 5])
            captured <- capturePublish firstConn "SERVICE.RECONNECT"
            Network.Socket.close firstConn
            (secondConn, _) <- accept sock
            void (completeHandshake secondConn)
            let replySubCommand = BS.concat ["SUB ", capturedInbox captured]
            pingBox <- newEmptyMVar
            pingWithCallback client (putMVar pingBox ())
            expectNoClientBytesBefore
              secondConn
              (BS.isInfixOf replySubCommand)
              "PING\r\n"
              "reply inbox was resubscribed"
            sendAll secondConn "PONG\r\n"
            expectMVar "PING failed after reconnect" pingBox
            close client []
            Network.Socket.close secondConn
        Network.Socket.close sock

    describe "jetstream protocol integration" $ do
      around withClient $ do
        it "sends stream create options as a JetStream API request" $ \(serv, client, _, _) -> do
          let Right jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.create (JetStream.streams jetStream)
              "PROTO_STREAM"
              ["PROTO.SUBJECT"]
              [ JetStream.withStorage JetStream.MemoryStorage
              , JetStream.withMaxMessages 1
              , JetStream.withDiscard JetStream.DiscardNew
              ]
              []
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
          let Right jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.putConsumer
              (JetStream.consumers jetStream)
              "PROTO_STREAM"
              JetStream.ConsumerCreate
              (JetStream.DurableConsumer "PROTO_CONSUMER")
              JetStream.PullConsumer
              [ JetStream.withConsumerDeliverPolicy JetStream.DeliverAll
              , JetStream.withConsumerAckPolicy JetStream.AckExplicit
              , JetStream.withConsumerFilter
                  (JetStream.ConsumerFilterSubjects ["PROTO.A", "PROTO.B"])
              ]
              []
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
          let Right jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.putConsumer
              (JetStream.consumers jetStream)
              "PROTO_STREAM"
              JetStream.ConsumerCreate
              (JetStream.NamedConsumer "PROTO_PUSH")
              (JetStream.PushConsumer "PROTO.DELIVER")
              [ JetStream.withConsumerDeliverGroup "workers"
              , JetStream.withConsumerAckPolicy JetStream.AckExplicit
              ]
              []
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
          let Right jetStream = newJetStream client []
          Right subscription <- JetStream.consumePush
            (JetStream.messages jetStream)
            "PROTO.DELIVER"
            [JetStream.withPushQueueGroup "workers"]
            []
            (const (pure ()))
          expectClientCommand serv "SUB PROTO.DELIVER workers "
          void (JetStream.stopPushSubscription subscription)
          expectClientCommand serv "UNSUB "

        it "keeps push consumers alive after garbled inbound bytes" $ \(serv, client, _, _) -> do
          let Right jetStream = newJetStream client []
          delivered <- newEmptyMVar
          Right subscription <- JetStream.consumePush
            (JetStream.messages jetStream)
            "PROTO.DELIVER"
            []
            []
            (putMVar delivered)
          captured <- captureSubscription serv "PROTO.DELIVER"
          sendAll serv $
            BS.concat
              [ "garbled-prefix"
              , msgFrame "PROTO.DELIVER" (capturedSubSid captured) "payload"
              ]
          result <- timeout 1000000 (takeMVar delivered)
          void (JetStream.stopPushSubscription subscription)
          case result of
            Nothing ->
              expectationFailure "push message was not delivered after garbled bytes"
            Just message ->
              JetStream.messagePayload message `shouldBe` "payload"

        it "creates ordered consumers with the ordered defaults" $ \(serv, client, _, _) -> do
          let Right jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.createOrderedConsumer
              (JetStream.messages jetStream)
              "PROTO_STREAM"
              [ JetStream.withOrderedConsumerNamePrefix "PROTO_ORDERED"
              , JetStream.withOrderedConsumerDeliverPolicy (JetStream.DeliverByStartSequence 3)
              ]
              []
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
          let Right jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            response <- JetStream.fetch (JetStream.messages jetStream)
              "PROTO_STREAM"
              "PROTO_CONSUMER"
              [ JetStream.withFetchBatch 1
              , JetStream.withFetchWait (JetStream.FetchNoWaitMicros 1000000)
              ]
              []
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
            Just (Left err) ->
              expectationFailure ("fetch failed: " ++ show err)
            Just (Right response) -> do
              JetStream.pullResponseMessages response `shouldBe` []
              JetStream.pullResponseStatus response
                `shouldBe` Just (JetStream.PullNoMessages (Just "No Messages"))

        it "sends account info requests" $ \(serv, client, _, _) -> do
          let Right jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.accountInfo (JetStream.management jetStream) []
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

        it "returns a JetStream timeout when admin requests receive no response" $ \(serv, client, _, _) -> do
          let Right jetStream = newJetStream client [withRequestTimeoutMicros 1000]
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.accountInfo (JetStream.management jetStream) []
            putMVar done result
          captured <- capturePublish serv "$JS.API.INFO"
          capturedPayload captured `shouldBe` ""
          result <- timeout 1000000 (takeMVar done)
          case result of
            Nothing ->
              expectationFailure "account info timeout did not complete"
            Just (Left JetStream.JetStreamTimeout) ->
              pure ()
            Just other ->
              expectationFailure ("unexpected account info timeout result: " ++ show other)

        it "sends stream message get and delete requests" $ \(serv, client, _, _) -> do
          let Right jetStream = newJetStream client []
          fetched <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.getMessage
              (JetStream.streams jetStream)
              "PROTO_STREAM"
              (JetStream.StreamMessageBySequence 2)
              []
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
              []
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
          let Right jetStream = newJetStream client []
          done <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.putConsumer
              (JetStream.consumers jetStream)
              "PROTO_STREAM"
              JetStream.ConsumerUpdate
              (JetStream.DurableConsumer "PROTO_CONSUMER")
              JetStream.PullConsumer
              [ JetStream.withConsumerDescription "updated"
              , JetStream.withConsumerAckPolicy JetStream.AckExplicit
              ]
              []
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
          let Right jetStream = newJetStream client []
              pauseUntil = read "2024-01-01 00:00:01 UTC"
          paused <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.pauseConsumer
              (JetStream.consumers jetStream)
              "PROTO_STREAM"
              "PROTO_CONSUMER"
              pauseUntil
              []
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
              []
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
        Right subscription <- subscribeOnce client "foo" [withSubscriptionExpiry 30] (const (pure ()))
        unsubscribe client subscription [] `shouldReturn` Right ()
        result <- timeout 1000000 (close client [])
        case result of
          Nothing -> expectationFailure "close blocked after unsubscribe"
          Just () -> pure ()
    it "early replies do not block close for tracked expiries" $ do
      bracket startClient stopClientSafely $ \(serv, client, _, _) -> do
        replyBox <- newEmptyMVar
        Right subscription <- subscribeOnce client "foo" [withSubscriptionExpiry 30] (putMVar replyBox)
        let sid = subscriptionSid subscription
        sendAll serv (msgFrame "foo" sid "bar")
        reply <- timeout 1000000 (takeMVar replyBox)
        case reply of
          Nothing  -> expectationFailure "reply callback did not run"
          Just msg -> payload msg `shouldBe` "bar"
        result <- timeout 1000000 (close client [])
        case result of
          Nothing -> expectationFailure "close blocked after reply"
          Just () -> pure ()
    it "refreshes auth handlers after a rejected connection attempt" $ do
      (p, sock) <- openFreePort
      listen sock 2
      captures <- newEmptyTMVarIO
      secondConnection <- newEmptyTMVarIO
      void . forkIO $ do
        (firstConn, _) <- accept sock
        sendAll firstConn defaultINFO
        firstConnect <- recv firstConn 4096
        sendAll firstConn "-ERR 'Authorization Violation'\r\n"
        (secondConn, _) <- accept sock
        secondConnect <- completeHandshake secondConn
        atomically $ do
          putTMVar captures (firstConnect, secondConnect)
          putTMVar secondConnection secondConn
      handlerCalls <- newIORef (0 :: Int)
      let tokenHandler = do
            call <- atomicModifyIORef' handlerCalls $ \current ->
              let next = current + 1
              in (next, next)
            pure (Right ("token-" <> C.pack (show call)))
          configOptions =
            [ withAuthTokenHandler tokenHandler
            , withConnectionAttempts 2
            , withConnectName "rotating-auth-client"
            ]
      client <- newClientOrFail [("127.0.0.1", p)] configOptions
      (firstConnect, secondConnect) <- atomically $ takeTMVar captures
      firstConnect `shouldSatisfy` BS.isInfixOf "\"token-1\""
      secondConnect `shouldSatisfy` BS.isInfixOf "\"token-2\""
      close client []
      Network.Socket.close =<< atomically (takeTMVar secondConnection)
      Network.Socket.close sock

    it "retries when the server disconnects before INFO" $ do
      (p, sock) <- openFreePort
      listen sock 2
      serverConn <- newEmptyTMVarIO
      clientVar <- newEmptyMVar
      void . forkIO $ do
        (firstConn, _) <- accept sock
        Network.Socket.close firstConn
        (secondConn, _) <- accept sock
        void (completeHandshake secondConn)
        atomically $ putTMVar serverConn secondConn
      void . forkIO $ do
        let configOptions =
              [ withMinimumLogLevel Debug
              , withConnectionAttempts 2
              , withConnectName "test-client"
              ]
        c <- newClientOrFail [("127.0.0.1", p)] configOptions
        putMVar clientVar c
      clientResult <- timeout 1000000 (takeMVar clientVar)
      case clientResult of
        Nothing -> expectationFailure "client did not recover after disconnect before INFO"
        Just client -> do
          secondConn <- atomically $ takeTMVar serverConn
          wg <- newWaitGroup 1
          pingWithCallback client $ done wg
          expectClientCommand secondConn "PING\r\n"
          sendAll secondConn "PONG\r\n"
          wait wg
          close client []
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
        c <- newClientOrFail [("127.0.0.1", p)] configOptions
        putMVar clientVar c
      (firstConn, _) <- accept sock
      void (completeHandshake firstConn)
      clientResult <- timeout 1000000 (takeMVar clientVar)
      case clientResult of
        Nothing -> expectationFailure "client did not connect"
        Just client -> do
          Right subscription <- subscribe client "JOBS" [withQueueGroup "WORKERS"] (const (pure ()))
          let sid = subscriptionSid subscription
          expectQueuedSub firstConn "JOBS" "WORKERS" sid
          Network.Socket.close firstConn
          (secondConn, _) <- accept sock
          void (completeHandshake secondConn)
          expectQueuedSub secondConn "JOBS" "WORKERS" sid
          close client []
          Network.Socket.close secondConn
      Network.Socket.close sock
    it "resubscribes JetStream push consumers after reconnect" $ do
      (p, sock) <- openFreePort
      listen sock 2
      clientVar <- newEmptyMVar
      void . forkIO $ do
        let configOptions =
              [ withMinimumLogLevel Debug
              , withConnectionAttempts 2
              , withConnectName "test-client"
              ]
        c <- newClientOrFail [("127.0.0.1", p)] configOptions
        putMVar clientVar c
      (firstConn, _) <- accept sock
      void (completeHandshake firstConn)
      clientResult <- timeout 1000000 (takeMVar clientVar)
      case clientResult of
        Nothing ->
          expectationFailure "client did not connect"
        Just client -> do
          let Right jetStream = newJetStream client []
          delivered <- newEmptyMVar
          Right subscription <- JetStream.consumePush
            (JetStream.messages jetStream)
            "PROTO.DELIVER"
            []
            []
            (putMVar delivered)
          firstSub <- captureSubscription firstConn "PROTO.DELIVER"
          Network.Socket.close firstConn
          (secondConn, _) <- accept sock
          void (completeHandshake secondConn)
          secondSub <- captureSubscription secondConn "PROTO.DELIVER"
          capturedSubSid secondSub `shouldBe` capturedSubSid firstSub
          sendAll secondConn $
            msgFrame "PROTO.DELIVER" (capturedSubSid secondSub) "after-reconnect"
          result <- timeout 1000000 (takeMVar delivered)
          case result of
            Nothing ->
              expectationFailure "push message was not delivered after reconnect"
            Just message ->
              JetStream.messagePayload message `shouldBe` "after-reconnect"
          void (JetStream.stopPushSubscription subscription)
          close client []
          Network.Socket.close secondConn
      Network.Socket.close sock
    it "ordered fetch returns after disconnect and recovers on reconnect" $ do
      (p, sock) <- openFreePort
      listen sock 2
      clientVar <- newEmptyMVar
      void . forkIO $ do
        let configOptions =
              [ withMinimumLogLevel Debug
              , withConnectionAttempts 2
              , withConnectName "test-client"
              ]
        c <- newClientOrFail [("127.0.0.1", p)] configOptions
        putMVar clientVar c
      (firstConn, _) <- accept sock
      void (completeHandshake firstConn)
      clientResult <- timeout 1000000 (takeMVar clientVar)
      case clientResult of
        Nothing ->
          expectationFailure "client did not connect"
        Just client -> do
          let Right jetStream = newJetStream client []
          orderedVar <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.createOrderedConsumer
              (JetStream.messages jetStream)
              "PROTO_STREAM"
              [JetStream.withOrderedConsumerNamePrefix "PROTO_ORDERED"]
              []
            putMVar orderedVar result
          createInitial <- capturePublish firstConn "$JS.API.CONSUMER.CREATE.PROTO_STREAM.PROTO_ORDERED_1"
          replyToCapturedPublish firstConn createInitial $
            protoOrderedConsumerInfoResponseFor "PROTO_ORDERED_1"
          orderedResult <- timeout 1000000 (takeMVar orderedVar)
          ordered <- case orderedResult of
            Nothing ->
              expectationFailure "ordered consumer create did not complete" >>
                fail "ordered consumer create did not complete"
            Just (Left err) ->
              expectationFailure ("ordered consumer create failed: " ++ show err) >>
                fail "ordered consumer create failed"
            Just (Right ordered) ->
              pure ordered

          fetchVar <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.fetchOrdered ordered
              [ JetStream.withFetchBatch 1
              , JetStream.withFetchWait (JetStream.FetchExpiresMicros 100000)
              ]
              []
            putMVar fetchVar result
          deleteCurrent <- capturePublish firstConn "$JS.API.CONSUMER.DELETE.PROTO_STREAM.PROTO_ORDERED_1"
          replyToCapturedPublish firstConn deleteCurrent protoDeleteConsumerResponse
          createNext <- capturePublish firstConn "$JS.API.CONSUMER.CREATE.PROTO_STREAM.PROTO_ORDERED_2"
          replyToCapturedPublish firstConn createNext $
            protoOrderedConsumerInfoResponseFor "PROTO_ORDERED_2"
          _nextRequest <- capturePublish firstConn "$JS.API.CONSUMER.MSG.NEXT.PROTO_STREAM.PROTO_ORDERED_2"
          Network.Socket.close firstConn
          fetchResult <- timeout 1000000 (takeMVar fetchVar)
          case fetchResult of
            Nothing ->
              expectationFailure "ordered fetch did not return after disconnect"
            Just (Left JetStream.JetStreamTimeout) ->
              pure ()
            Just other ->
              expectationFailure ("unexpected ordered fetch result after disconnect: " ++ show other)

          (secondConn, _) <- accept sock
          void (completeHandshake secondConn)
          infoVar <- newEmptyMVar
          void . forkIO $ do
            result <- JetStream.orderedConsumerInfo ordered []
            putMVar infoVar result
          infoRequest <- capturePublish secondConn "$JS.API.CONSUMER.INFO.PROTO_STREAM.PROTO_ORDERED_2"
          replyToCapturedPublish secondConn infoRequest $
            protoOrderedConsumerInfoResponseFor "PROTO_ORDERED_2"
          infoResult <- timeout 1000000 (takeMVar infoVar)
          case infoResult of
            Nothing ->
              expectationFailure "ordered consumer info did not complete after reconnect"
            Just (Left err) ->
              expectationFailure ("ordered consumer info failed after reconnect: " ++ show err)
            Just (Right info) ->
              JetStream.consumerInfoName info `shouldBe` "PROTO_ORDERED_2"
          close client []
          Network.Socket.close secondConn
      Network.Socket.close sock
    around (withClientWith [withMessageLimit (64 * 1024), withCallbackConcurrency 4]) $ do
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
              let payloadLen = BS.length (payload msg)
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
        Right subscription <- subscribe client subject [] handleMsg
        let sid = subscriptionSid subscription
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
