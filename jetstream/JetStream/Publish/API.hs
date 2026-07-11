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

newtype PublishAPI = PublishAPI { publish :: Subject -> Payload -> [PublishOption] -> IO (Either JetStreamError PublishAck) }
