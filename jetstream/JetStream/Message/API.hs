module JetStream.Message.API
  ( MessageAPI (..)
  , FetchOption
  , FetchWait (..)
  , Headers
  , Message (..)
  , OrderedConsumer (..)
  , OrderedConsumerOption
  , PushConsumeOption
  , PushSubscription (..)
  , PullResponse (..)
  , PullStatus (..)
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

import           JetStream.Error         (JetStreamError)
import           JetStream.Message.Types
    ( FetchOption
    , FetchWait (..)
    , Headers
    , Message (..)
    , OrderedConsumer (..)
    , OrderedConsumerOption
    , PullResponse (..)
    , PullStatus (..)
    , PushConsumeOption
    , PushSubscription (..)
    , withFetchBatch
    , withFetchWait
    , withOrderedConsumerDeliverPolicy
    , withOrderedConsumerFilter
    , withOrderedConsumerHeadersOnly
    , withOrderedConsumerInactiveThreshold
    , withOrderedConsumerNamePrefix
    , withOrderedConsumerReplayPolicy
    , withPushQueueGroup
    )
import           JetStream.Types         (ConsumerName, StreamName, Subject)

-- | Pull-consumer message operations.
data MessageAPI = MessageAPI
                    { fetch :: StreamName -> ConsumerName -> [FetchOption] -> IO (Either JetStreamError PullResponse)
                      -- ^ Fetch messages for a pull consumer.
                    , consumePush :: Subject -> [PushConsumeOption] -> (Message -> IO ()) -> IO PushSubscription
                      -- ^ Subscribe to a push consumer deliver subject.
                    , createOrderedConsumer :: StreamName -> [OrderedConsumerOption] -> IO (Either JetStreamError OrderedConsumer)
                      -- ^ Create a client-managed ordered pull consumer.
                    , ack :: Message -> IO (Either JetStreamError ())
                      -- ^ Acknowledge successful message processing.
                    , nak :: Message -> IO (Either JetStreamError ())
                      -- ^ Negatively acknowledge a message.
                    , inProgress :: Message -> IO (Either JetStreamError ())
                      -- ^ Tell the server that message processing is still in progress.
                    , term :: Message -> IO (Either JetStreamError ())
                    -- ^ Terminate message redelivery.
                    }
