module JetStream.Publish.API
  ( PublishAPI (..)
  , PublishAck (..)
  , PublishExpectation (..)
  , PublishOption
  , withMsgId
  , withExpectedStream
  , withPublishExpectation
  , withHeaders
  ) where

import           JetStream.Error         (JetStreamError)
import           JetStream.Publish.Types
    ( PublishAck (..)
    , PublishExpectation (..)
    , PublishOption
    , withExpectedStream
    , withHeaders
    , withMsgId
    , withPublishExpectation
    )
import           JetStream.Types         (Payload, Subject)

-- | Publish operations for JetStream subjects.
--
-- The 'publish' field publishes a message and waits for a JetStream publish
-- acknowledgement.
newtype PublishAPI = PublishAPI { publish :: Subject -> Payload -> [PublishOption] -> IO (Either JetStreamError PublishAck) }
