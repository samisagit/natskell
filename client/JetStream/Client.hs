-- | High-level client implementation for JetStream.
module JetStream.Client
  ( newJetStream
  , JetStream
  , JetStreamOption
  , JetStreamRequestOption
  , JetStreamConfigError (..)
  , withDomain
  , withRequestTimeout
  , withRequestTimeoutMicros
  ) where

import qualified API                  as Nats
import           JetStream.API        (JetStream)
import qualified JetStream.Consumer   as Consumer
import qualified JetStream.Management as Management
import qualified JetStream.Message    as Message
import           JetStream.Options
    ( JetStream (..)
    , JetStreamConfigError (..)
    , JetStreamOption
    , tryNewJetStreamContext
    , withDomain
    , withRequestTimeout
    , withRequestTimeoutMicros
    )
import qualified JetStream.Publish    as Publish
import qualified JetStream.Stream     as Stream
import           JetStream.Types      (JetStreamRequestOption)

-- | Build JetStream capabilities from an existing NATS client.
newJetStream :: Nats.Client -> [JetStreamOption] -> Either JetStreamConfigError JetStream
newJetStream client options = do
  ctx <- tryNewJetStreamContext client options
  let consumerAPI = Consumer.consumerAPI ctx
  pure JetStream
    { streams = Stream.streamAPI ctx
    , consumers = consumerAPI
    , publisher = Publish.publishAPI ctx
    , messages = Message.messageAPI ctx consumerAPI
    , management = Management.managementAPI ctx
    }
