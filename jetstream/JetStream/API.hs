module JetStream.API
  ( JetStream (..)
  ) where

import           JetStream.Consumer.API (ConsumerAPI)
import           JetStream.Message.API  (MessageAPI)
import           JetStream.Publish.API  (PublishAPI)
import           JetStream.Stream.API   (StreamAPI)

data JetStream = JetStream
                   { streams   :: StreamAPI
                   , consumers :: ConsumerAPI
                   , publisher :: PublishAPI
                   , messages  :: MessageAPI
                   }

