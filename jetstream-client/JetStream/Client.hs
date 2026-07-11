-- | High-level client implementation for JetStream.
module JetStream.Client
  ( newJetStream
  , JetStream (..)
  , JetStreamOption
  , withDomain
  , withRequestTimeoutMicros
  ) where

import qualified API                as Nats
import           JetStream.API      (JetStream (..))
import qualified JetStream.Consumer as Consumer
import qualified JetStream.Message  as Message
import           JetStream.Options
    ( JetStreamOption
    , newJetStreamContext
    , withDomain
    , withRequestTimeoutMicros
    )
import qualified JetStream.Publish  as Publish
import qualified JetStream.Stream   as Stream

-- | Build a JetStream capability record from an existing NATS client.
newJetStream :: Nats.Client -> [JetStreamOption] -> JetStream
newJetStream client options =
  let ctx = newJetStreamContext client options
  in JetStream
    { streams = Stream.streamAPI ctx
    , consumers = Consumer.consumerAPI ctx
    , publisher = Publish.publishAPI ctx
    , messages = Message.messageAPI ctx
    }
