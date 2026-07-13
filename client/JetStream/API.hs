-- | Capability record for the JetStream client surface.
module JetStream.API
  ( JetStream (..)
  , AccountAPIStats (..)
  , AccountInfo (..)
  , AccountLimits (..)
  , AccountTier (..)
  , JetStreamApiError (..)
  , JetStreamError (..)
  , StreamName
  , ConsumerName
  , Subject
  , Payload
  , module JetStream.Consumer.API
  , module JetStream.Message.API
  , module JetStream.Publish.API
  , module JetStream.Stream.API
  ) where

import           JetStream.Consumer.API
import           JetStream.Error
    ( JetStreamApiError (..)
    , JetStreamError (..)
    )
import           JetStream.Message.API
import           JetStream.Publish.API
import           JetStream.Stream.API
import           JetStream.Types
    ( AccountAPIStats (..)
    , AccountInfo (..)
    , AccountLimits (..)
    , AccountTier (..)
    , ConsumerName
    , Payload
    , StreamName
    , Subject
    )

-- | JetStream capabilities for streams, consumers, publishing, and pull
-- message workflows.
data JetStream = JetStream
                   { streams     :: StreamAPI
                     -- ^ Manage streams and inspect stream state.
                   , consumers   :: ConsumerAPI
                     -- ^ Manage stream consumers.
                   , publisher   :: PublishAPI
                     -- ^ Publish messages to JetStream subjects.
                   , messages    :: MessageAPI
                     -- ^ Fetch and acknowledge pull-consumer messages.
                   , accountInfo :: IO (Either JetStreamError AccountInfo)
                   -- ^ Fetch account-level JetStream usage and limits.
                   }
