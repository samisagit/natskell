module JetStream.Stream.API
  ( StreamAPI (..)
  , module JetStream.Stream.Types
  ) where

import           JetStream.Error        (JetStreamError)
import           JetStream.Stream.Types
import           JetStream.Types        (StreamName)

-- | Stream management operations.
data StreamAPI = StreamAPI
                   { create :: StreamConfig -> IO (Either JetStreamError StreamInfo)
                     -- ^ Create a stream.
                   , update :: StreamConfig -> IO (Either JetStreamError StreamInfo)
                     -- ^ Update an existing stream configuration.
                   , info :: StreamName -> IO (Either JetStreamError StreamInfo)
                     -- ^ Fetch stream configuration and state.
                   , delete :: StreamName -> IO (Either JetStreamError DeleteStreamResponse)
                     -- ^ Delete a stream.
                   , purge :: StreamName -> PurgeStreamRequest -> IO (Either JetStreamError PurgeStreamResponse)
                     -- ^ Purge messages from a stream.
                   , list :: StreamListRequest -> IO (Either JetStreamError StreamListResponse)
                     -- ^ List streams and their metadata.
                   , names :: StreamNamesRequest -> IO (Either JetStreamError StreamNamesResponse)
                   -- ^ List stream names.
                   }
