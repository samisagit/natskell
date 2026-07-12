{-# LANGUAGE OverloadedStrings #-}

module JetStream.Publish.Types
  ( PublishAck (..)
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
import           JetStream.Types       (StreamName)

data PublishAck = PublishAck
                    { publishAckStream    :: StreamName
                    , publishAckSequence  :: Word64
                    , publishAckDuplicate :: Maybe Bool
                    , publishAckDomain    :: Maybe BS.ByteString
                    }
  deriving (Eq, Show)

data PublishExpectation = ExpectedLastSequence Word64
                        | ExpectedLastSubjectSequence Word64
                        | ExpectedLastMsgId BS.ByteString
  deriving (Eq, Show)

instance FromJSON PublishAck where
  parseJSON = withObject "PublishAck" $ \object -> do
    streamName <- object .: "stream"
    sequence <- object .: "seq"
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
          ("Nats-Expected-Last-Sequence", renderWord64 sequenceNumber)
        ExpectedLastSubjectSequence sequenceNumber ->
          ("Nats-Expected-Last-Subject-Sequence", renderWord64 sequenceNumber)
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
  publishConfigHeaders (foldr ($) defaultPublishConfig options)

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
