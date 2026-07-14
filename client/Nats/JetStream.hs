-- | JetStream client construction and capabilities.
--
-- Stream, consumer, publish, and message-specific APIs are also available
-- from the corresponding @Nats.JetStream.*@ modules.
module Nats.JetStream
  ( module JetStream.API
  , newJetStream
  , JetStreamOption
  , JetStreamConfigError (..)
  , withDomain
  , withRequestTimeoutMicros
  ) where

import           JetStream.API
import           JetStream.Client
    ( JetStreamConfigError (..)
    , JetStreamOption
    , newJetStream
    , withDomain
    , withRequestTimeoutMicros
    )
