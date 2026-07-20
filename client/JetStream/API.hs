-- | Capability record for the JetStream client surface.
module JetStream.API
  ( JetStream
  , streams
  , consumers
  , publisher
  , messages
  , management
  , keyValues
  , JetStreamRequestOption
  , withRequestTimeout
  , JetStreamApiError
  , apiErrorCode
  , apiErrorCodeDetail
  , apiErrorDescription
  , JetStreamError (..)
  , module JetStream.API.Consumer
  , module JetStream.API.KeyValue
  , module JetStream.API.Management
  , module JetStream.API.Message
  , module JetStream.API.Publish
  , module JetStream.API.Stream
  ) where

import           JetStream.API.Consumer
import           JetStream.API.KeyValue
import           JetStream.API.Management
import           JetStream.API.Message
import           JetStream.API.Publish
import           JetStream.API.Stream
import           JetStream.Error
    ( JetStreamApiError
    , JetStreamError (..)
    , apiErrorCode
    , apiErrorCodeDetail
    , apiErrorDescription
    )
import           JetStream.Options
    ( JetStream
    , consumers
    , keyValues
    , management
    , messages
    , publisher
    , streams
    )
import           JetStream.Types
    ( JetStreamRequestOption
    , withRequestTimeout
    )
