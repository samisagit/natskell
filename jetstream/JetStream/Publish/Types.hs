{-# LANGUAGE OverloadedStrings #-}

module JetStream.Publish.Types
  ( PublishAPI (..)
  , PublishAck (..)
  , PublishExpectation (..)
  , PublishOption
  , withMsgId
  , withExpectedStream
  , withPublishExpectation
  , withHeaders
  , publishHeaders
  ) where

import           Data.Aeson            (FromJSON (..), withObject, (.:), (.:?))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Text.Encoding    (encodeUtf8)
import           Data.Word             (Word64)
import           JetStream.Error       (JetStreamError)
import           JetStream.Types
    ( JetStreamRequestOption
    , Payload
    , Sequence
    , StreamName
    , Subject
    , sequenceFromWord64
    , sequenceToWord64
    )

-- | Publish operations. The public API hides the constructor so additional
-- publish modes can be introduced without changing downstream code.
newtype PublishAPI = PublishAPI { publish :: Subject -> Payload -> [PublishOption] -> [JetStreamRequestOption] -> IO (Either JetStreamError PublishAck) }

data PublishAck = PublishAck
                    { publishAckStream    :: StreamName
                    , publishAckSequence  :: Sequence
                    , publishAckDuplicate :: Maybe Bool
                    , publishAckDomain    :: Maybe BS.ByteString
                    }
  deriving (Eq, Show)

data PublishExpectation = ExpectedLastSequence Sequence
                        | ExpectedLastSubjectSequence Sequence
                        | ExpectedLastMsgId BS.ByteString
  deriving (Eq, Show)

instance FromJSON PublishAck where
  parseJSON = withObject "PublishAck" $ \object -> do
    streamName <- object .: "stream"
    sequence <- sequenceFromWord64 <$> (object .: "seq")
    duplicate <- object .:? "duplicate"
    domainName <- object .:? "domain"
    pure PublishAck
      { publishAckStream = encodeUtf8 streamName
      , publishAckSequence = sequence
      , publishAckDuplicate = duplicate
      , publishAckDomain = encodeUtf8 <$> domainName
      }

type PublishOption = PublishConfig -> PublishConfig

newtype PublishConfig = PublishConfig { publishConfigHeaders :: [(BS.ByteString, BS.ByteString)] }

defaultPublishConfig :: PublishConfig
defaultPublishConfig =
  PublishConfig
    { publishConfigHeaders = []
    }

withMsgId :: BS.ByteString -> PublishOption
withMsgId =
  putHeader "Nats-Msg-Id"

withExpectedStream :: StreamName -> PublishOption
withExpectedStream =
  putHeader "Nats-Expected-Stream"

withPublishExpectation :: PublishExpectation -> PublishOption
withPublishExpectation expectation =
  putExpectationHeader key value
  where
    (key, value) =
      case expectation of
        ExpectedLastSequence sequenceNumber ->
          ("Nats-Expected-Last-Sequence", renderSequence sequenceNumber)
        ExpectedLastSubjectSequence sequenceNumber ->
          ("Nats-Expected-Last-Subject-Sequence", renderSequence sequenceNumber)
        ExpectedLastMsgId msgId ->
          ("Nats-Expected-Last-Msg-Id", msgId)

withHeaders :: [(BS.ByteString, BS.ByteString)] -> PublishOption
withHeaders headers config =
  config
    { publishConfigHeaders =
        filter (not . isPublishExpectationHeader . fst) headers
          ++ publishConfigHeaders config
    }

publishHeaders :: [PublishOption] -> [(BS.ByteString, BS.ByteString)]
publishHeaders options =
  publishConfigHeaders (foldl (flip ($)) defaultPublishConfig options)

putHeader :: BS.ByteString -> BS.ByteString -> PublishOption
putHeader key value config =
  config
    { publishConfigHeaders =
        (key, value) : filter ((/= key) . fst) (publishConfigHeaders config)
    }

putExpectationHeader :: BS.ByteString -> BS.ByteString -> PublishOption
putExpectationHeader key value config =
  config
    { publishConfigHeaders =
        (key, value) : filter (not . isPublishExpectationHeader . fst) (publishConfigHeaders config)
    }

isPublishExpectationHeader :: BS.ByteString -> Bool
isPublishExpectationHeader key =
  key `elem`
    [ "Nats-Expected-Last-Sequence"
    , "Nats-Expected-Last-Subject-Sequence"
    , "Nats-Expected-Last-Msg-Id"
    ]

renderWord64 :: Word64 -> BS.ByteString
renderWord64 =
  BC.pack . show

renderSequence :: Sequence -> BS.ByteString
renderSequence =
  renderWord64 . sequenceToWord64
