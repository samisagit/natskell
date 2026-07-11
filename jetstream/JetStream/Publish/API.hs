module JetStream.Publish.API
  ( PublishAPI (..)
  , PublishAck (..)
  , PublishOption
  , withMsgId
  , withExpectedStream
  , withExpectedLastSequence
  , withExpectedLastSubjectSequence
  , withExpectedLastMsgId
  , withHeaders
  ) where

import           JetStream.Error         (JetStreamError)
import           JetStream.Publish.Types
    ( PublishAck (..)
    , PublishOption
    , withExpectedLastMsgId
    , withExpectedLastSequence
    , withExpectedLastSubjectSequence
    , withExpectedStream
    , withHeaders
    , withMsgId
    )
import           JetStream.Types         (Payload, Subject)

-- | Publish operations for JetStream subjects.
--
-- The 'publish' field publishes a message and waits for a JetStream publish
-- acknowledgement.
newtype PublishAPI = PublishAPI { publish :: Subject -> Payload -> [PublishOption] -> IO (Either JetStreamError PublishAck) }
