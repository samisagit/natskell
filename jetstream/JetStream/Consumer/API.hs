module JetStream.Consumer.API
  ( ConsumerAPI (..)
  , module JetStream.Consumer.Types
  ) where

import           JetStream.Consumer.Types
import           JetStream.Error          (JetStreamError)
import           JetStream.Types          (ConsumerName, StreamName)

data ConsumerAPI = ConsumerAPI
                     { createConsumer :: StreamName -> ConsumerConfig -> IO (Either JetStreamError ConsumerInfo)
                     , createDurableConsumer :: StreamName -> ConsumerName -> ConsumerConfig -> IO (Either JetStreamError ConsumerInfo)
                     , consumerInfo :: StreamName -> ConsumerName -> IO (Either JetStreamError ConsumerInfo)
                     , deleteConsumer :: StreamName -> ConsumerName -> IO (Either JetStreamError DeleteConsumerResponse)
                     , listConsumers :: StreamName -> ConsumerListRequest -> IO (Either JetStreamError ConsumerListResponse)
                     , consumerNames :: StreamName -> ConsumerNamesRequest -> IO (Either JetStreamError ConsumerNamesResponse)
                     }

