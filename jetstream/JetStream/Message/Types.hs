{-# LANGUAGE OverloadedStrings #-}

module JetStream.Message.Types
  ( MessageAPI (..)
  , AckVerb (..)
  , NakDelay (..)
  , FetchOption
  , FetchWait (..)
  , Headers
  , Message (..)
  , MessageMetadata (..)
  , OrderedConsumer (..)
  , OrderedConsumerConfig (..)
  , OrderedConsumerOption
  , PushConsumeConfig (..)
  , PushConsumeOption
  , PushSubscription (..)
  , PullRequest (..)
  , PullResponse (..)
  , PullStatus (..)
  , ackPayload
  , classifyStatusHeaders
  , defaultPullRequest
  , descriptionHeader
  , pullRequest
  , inProgressPayload
  , isStatusMessage
  , messageMetadata
  , nakPayload
  , nakDelay
  , nakDelayPayload
  , orderedConsumerConfig
  , pullRequestPayload
  , pushConsumeConfig
  , statusHeader
  , termPayload
  , withFetchBatch
  , withFetchWait
  , withOrderedConsumerDeliverPolicy
  , withOrderedConsumerFilter
  , withOrderedConsumerHeadersOnly
  , withOrderedConsumerInactiveThreshold
  , withOrderedConsumerNamePrefix
  , withOrderedConsumerReplayPolicy
  , withPushQueueGroup
  ) where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BC
import           Data.Char                (isDigit, toLower)
import           Data.Int                 (Int64)
import           Data.Maybe               (isJust)
import           Data.Time.Clock          (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock.POSIX    as Time
import           Data.Word                (Word64)
import           JetStream.Consumer.Types (ConsumerFilter, ConsumerInfo)
import           JetStream.Error          (JetStreamError)
import           JetStream.Types
    ( CallOption
    , ConsumerName
    , DeliverPolicy (..)
    , Headers
    , JetStreamRequestOption
    , Payload
    , ReplayPolicy
    , StreamName
    , Subject
    , applyCallOptions
    )

-- | Message consumption and acknowledgement operations. The public module
-- hides the constructor so this capability can grow additively.
data MessageAPI = MessageAPI
                    { fetch :: StreamName -> ConsumerName -> [FetchOption] -> [JetStreamRequestOption] -> IO (Either JetStreamError PullResponse)
                    , consumePush :: Subject -> [PushConsumeOption] -> [JetStreamRequestOption] -> (Message -> IO ()) -> IO (Either JetStreamError PushSubscription)
                    , createOrderedConsumer :: StreamName -> [OrderedConsumerOption] -> [JetStreamRequestOption] -> IO (Either JetStreamError OrderedConsumer)
                    , ack :: Message -> [JetStreamRequestOption] -> IO (Either JetStreamError ())
                    , ackSync :: Message -> [JetStreamRequestOption] -> IO (Either JetStreamError ())
                    , nak :: Message -> [JetStreamRequestOption] -> IO (Either JetStreamError ())
                    , nakWithDelay :: Message -> NakDelay -> [JetStreamRequestOption] -> IO (Either JetStreamError ())
                    , inProgress :: Message -> [JetStreamRequestOption] -> IO (Either JetStreamError ())
                    , term :: Message -> [JetStreamRequestOption] -> IO (Either JetStreamError ())
                    , termSync :: Message -> [JetStreamRequestOption] -> IO (Either JetStreamError ())
                    }

data PullRequest = PullRequest
                     { pullRequestBatch :: Int
                     , pullRequestWait  :: FetchWait
                     }
  deriving (Eq, Show)

data FetchWait = FetchExpiresMicros Int
               | FetchNoWaitMicros Int
  deriving (Eq, Show)

defaultPullRequest :: PullRequest
defaultPullRequest =
  PullRequest
    { pullRequestBatch = 1
    , pullRequestWait = FetchExpiresMicros 1000000
    }

type FetchOption = CallOption PullRequest

pullRequest :: [FetchOption] -> PullRequest
pullRequest options =
  applyCallOptions options defaultPullRequest

withFetchBatch :: Int -> FetchOption
withFetchBatch batch request =
  request { pullRequestBatch = batch }

withFetchWait :: FetchWait -> FetchOption
withFetchWait wait request =
  request { pullRequestWait = wait }

data PullResponse = PullResponse
                      { pullResponseMessages :: [Message]
                      , pullResponseStatus   :: Maybe PullStatus
                      }
  deriving (Eq, Show)

data Message = Message
                 { messageSubject :: Subject
                 , messagePayload :: Payload
                 , messageHeaders :: Maybe Headers
                 , messageReplyTo :: Maybe Subject
                 , messageStatus  :: Maybe PullStatus
                 }
  deriving (Eq, Show)

data MessageMetadata = MessageMetadata
                         { messageMetadataStream           :: StreamName
                         , messageMetadataConsumer         :: ConsumerName
                         , messageMetadataStreamSequence   :: Word64
                         , messageMetadataConsumerSequence :: Word64
                         , messageMetadataNumDelivered     :: Integer
                         , messageMetadataNumPending       :: Integer
                         , messageMetadataTimestamp        :: UTCTime
                         , messageMetadataDomain           :: Maybe ByteString
                         }
  deriving (Eq, Show)

newtype PushSubscription = PushSubscription { stopPushSubscription :: IO (Either JetStreamError ()) }

newtype PushConsumeConfig = PushConsumeConfig { pushConsumeQueueGroup :: Maybe Subject }

type PushConsumeOption = CallOption PushConsumeConfig

pushConsumeConfig :: [PushConsumeOption] -> PushConsumeConfig
pushConsumeConfig options =
  applyCallOptions options $
    PushConsumeConfig
      { pushConsumeQueueGroup = Nothing
      }

withPushQueueGroup :: Subject -> PushConsumeOption
withPushQueueGroup queueGroup config =
  config { pushConsumeQueueGroup = Just queueGroup }

data OrderedConsumer = OrderedConsumer
                         { orderedConsumerInfo :: [JetStreamRequestOption] -> IO (Either JetStreamError ConsumerInfo)
                         , fetchOrdered :: [FetchOption] -> [JetStreamRequestOption] -> IO (Either JetStreamError PullResponse)
                         , stopOrderedConsumer :: [JetStreamRequestOption] -> IO (Either JetStreamError ())
                         }

data OrderedConsumerConfig = OrderedConsumerConfig
                               { orderedConsumerNamePrefix :: Maybe ConsumerName
                               , orderedConsumerDeliverPolicy :: DeliverPolicy
                               , orderedConsumerFilter :: Maybe ConsumerFilter
                               , orderedConsumerReplayPolicy :: Maybe ReplayPolicy
                               , orderedConsumerInactiveThreshold :: Maybe NominalDiffTime
                               , orderedConsumerHeadersOnly :: Maybe Bool
                               }

type OrderedConsumerOption = CallOption OrderedConsumerConfig

orderedConsumerConfig :: [OrderedConsumerOption] -> OrderedConsumerConfig
orderedConsumerConfig options =
  applyCallOptions options $
    OrderedConsumerConfig
      { orderedConsumerNamePrefix = Nothing
      , orderedConsumerDeliverPolicy = DeliverAll
      , orderedConsumerFilter = Nothing
      , orderedConsumerReplayPolicy = Nothing
      , orderedConsumerInactiveThreshold = Nothing
      , orderedConsumerHeadersOnly = Nothing
      }

withOrderedConsumerNamePrefix :: ConsumerName -> OrderedConsumerOption
withOrderedConsumerNamePrefix namePrefix config =
  config { orderedConsumerNamePrefix = Just namePrefix }

withOrderedConsumerDeliverPolicy :: DeliverPolicy -> OrderedConsumerOption
withOrderedConsumerDeliverPolicy policy config =
  config { orderedConsumerDeliverPolicy = policy }

withOrderedConsumerFilter :: ConsumerFilter -> OrderedConsumerOption
withOrderedConsumerFilter consumerFilter config =
  config { orderedConsumerFilter = Just consumerFilter }

withOrderedConsumerReplayPolicy :: ReplayPolicy -> OrderedConsumerOption
withOrderedConsumerReplayPolicy replayPolicy config =
  config { orderedConsumerReplayPolicy = Just replayPolicy }

withOrderedConsumerInactiveThreshold :: NominalDiffTime -> OrderedConsumerOption
withOrderedConsumerInactiveThreshold inactiveThreshold config =
  config { orderedConsumerInactiveThreshold = Just inactiveThreshold }

withOrderedConsumerHeadersOnly :: Bool -> OrderedConsumerOption
withOrderedConsumerHeadersOnly headersOnly config =
  config { orderedConsumerHeadersOnly = Just headersOnly }

data PullStatus = PullNoMessages (Maybe ByteString)
                | PullRequestTimeout (Maybe ByteString)
                | PullStatusError ByteString (Maybe ByteString)
  deriving (Eq, Show)

data AckVerb = Ack | Nak | InProgress | Term
  deriving (Eq, Show)

-- | A positive, whole-nanosecond JetStream redelivery delay.
newtype NakDelay = NakDelay Int64
  deriving (Eq, Show)

-- | Build a delayed-NAK duration. Durations below one nanosecond, negative
-- durations, and values outside the JetStream signed 64-bit wire range are
-- rejected. Sub-nanosecond precision is rounded down.
nakDelay :: NominalDiffTime -> Maybe NakDelay
nakDelay duration
  | nanoseconds < 1 = Nothing
  | nanoseconds > toInteger (maxBound :: Int64) = Nothing
  | otherwise = Just (NakDelay (fromInteger nanoseconds))
  where
    nanoseconds = floor (toRational duration * 1000000000)

ackPayload :: AckVerb -> Payload
ackPayload Ack        = "+ACK"
ackPayload Nak        = "-NAK"
ackPayload InProgress = "+WPI"
ackPayload Term       = "+TERM"

nakPayload :: Payload
nakPayload = ackPayload Nak

nakDelayPayload :: NakDelay -> Payload
nakDelayPayload (NakDelay nanoseconds) =
  BS.concat
    [ nakPayload
    , " {\"delay\":"
    , BC.pack (show nanoseconds)
    , "}"
    ]

inProgressPayload :: Payload
inProgressPayload = ackPayload InProgress

termPayload :: Payload
termPayload = ackPayload Term

statusHeader :: Headers -> Maybe ByteString
statusHeader = lookupHeader "Status"

descriptionHeader :: Headers -> Maybe ByteString
descriptionHeader = lookupHeader "Description"

classifyStatusHeaders :: Maybe Headers -> Maybe PullStatus
classifyStatusHeaders Nothing = Nothing
classifyStatusHeaders (Just headers) =
  case statusHeader headers of
    Nothing     -> Nothing
    Just "404"  -> Just (PullNoMessages description)
    Just "408"  -> Just (PullRequestTimeout description)
    Just status -> Just (PullStatusError status description)
  where
    description = descriptionHeader headers

isStatusMessage :: Message -> Bool
isStatusMessage = isJust . messageStatus

messageMetadata :: Message -> Maybe MessageMetadata
messageMetadata message =
  messageReplyTo message >>= parseMetadataSubject

pullRequestPayload :: Int -> PullRequest -> Payload
pullRequestPayload requestedBatch request =
  BS.concat
    [ "{\"batch\":"
    , intBytes (max 1 requestedBatch)
    , noWaitField
    , expiresField
    , "}"
    ]
  where
    noWaitField
      | FetchNoWaitMicros _ <- pullRequestWait request = ",\"no_wait\":true"
      | otherwise = ""
    expiresField =
      case pullRequestWait request of
        FetchNoWaitMicros _ ->
          ""
        FetchExpiresMicros timeoutMicros
          | timeoutMicros <= 0 -> ""
          | otherwise ->
          BS.concat
            [ ",\"expires\":"
            , intBytes (timeoutMicros * 1000)
            ]

lookupHeader :: ByteString -> Headers -> Maybe ByteString
lookupHeader needle = go
  where
    normalizedNeedle = normalizeHeaderName needle
    go [] = Nothing
    go ((name, value):rest)
      | normalizeHeaderName name == normalizedNeedle = Just value
      | otherwise = go rest

normalizeHeaderName :: ByteString -> ByteString
normalizeHeaderName = BC.map toLower

intBytes :: Int -> ByteString
intBytes = BC.pack . show

parseMetadataSubject :: Subject -> Maybe MessageMetadata
parseMetadataSubject subject = do
  tokens <- normalizeMetadataTokens (BC.split '.' subject)
  stream <- tokenAt ackStreamTokenPos tokens
  consumer <- tokenAt ackConsumerTokenPos tokens
  delivered <- parseNum =<< tokenAt ackNumDeliveredTokenPos tokens
  streamSeq <- parseSequence =<< tokenAt ackStreamSeqTokenPos tokens
  consumerSeq <- parseSequence =<< tokenAt ackConsumerSeqTokenPos tokens
  timestamp <- nanosToTime <$> (parseNum =<< tokenAt ackTimestampTokenPos tokens)
  pending <- parseNum =<< tokenAt ackNumPendingTokenPos tokens
  let domain = nonEmpty =<< tokenAt ackDomainTokenPos tokens
  pure MessageMetadata
    { messageMetadataStream = stream
    , messageMetadataConsumer = consumer
    , messageMetadataStreamSequence = streamSeq
    , messageMetadataConsumerSequence = consumerSeq
    , messageMetadataNumDelivered = delivered
    , messageMetadataNumPending = pending
    , messageMetadataTimestamp = timestamp
    , messageMetadataDomain = domain
    }

normalizeMetadataTokens :: [ByteString] -> Maybe [ByteString]
normalizeMetadataTokens tokens
  | length tokens < 9 || (length tokens > 9 && length tokens < 11) =
      Nothing
  | take 2 tokens /= ["$JS", "ACK"] =
      Nothing
  | length tokens == 9 =
      Just (take ackDomainTokenPos tokens ++ ["", ""] ++ drop ackDomainTokenPos tokens)
  | otherwise =
      Just (normalizeDomain tokens)
  where
    normalizeDomain values =
      case tokenAt ackDomainTokenPos values of
        Just "_" ->
          take ackDomainTokenPos values ++ [""] ++ drop (ackDomainTokenPos + 1) values
        _ ->
          values

tokenAt :: Int -> [a] -> Maybe a
tokenAt index values =
  if index < length values
    then Just (values !! index)
    else Nothing

parseNum :: ByteString -> Maybe Integer
parseNum bytes
  | BS.null bytes = Just 0
  | BC.all isDigit bytes = Just (read (BC.unpack bytes))
  | otherwise = Nothing

parseSequence :: ByteString -> Maybe Word64
parseSequence bytes = do
  value <- parseNum bytes
  if value < 0 || value > toInteger (maxBound :: Word64)
    then Nothing
    else Just (fromInteger value)

nanosToTime :: Integer -> UTCTime
nanosToTime nanoseconds =
  Time.posixSecondsToUTCTime $
    fromRational (toRational nanoseconds / 1000000000)

nonEmpty :: ByteString -> Maybe ByteString
nonEmpty bytes
  | BS.null bytes = Nothing
  | otherwise = Just bytes

ackDomainTokenPos :: Int
ackDomainTokenPos = 2

ackStreamTokenPos :: Int
ackStreamTokenPos = 4

ackConsumerTokenPos :: Int
ackConsumerTokenPos = 5

ackNumDeliveredTokenPos :: Int
ackNumDeliveredTokenPos = 6

ackStreamSeqTokenPos :: Int
ackStreamSeqTokenPos = 7

ackConsumerSeqTokenPos :: Int
ackConsumerSeqTokenPos = 8

ackTimestampTokenPos :: Int
ackTimestampTokenPos = 9

ackNumPendingTokenPos :: Int
ackNumPendingTokenPos = 10
