module JetStream.Stream.API
  ( StreamAPI (..)
  , RetentionPolicy (..)
  , StorageType (..)
  , DiscardPolicy (..)
  , StreamConfig (..)
  , StreamConfigOption
  , withRetention
  , withStorage
  , withDiscard
  , withMaxMessages
  , withMaxBytes
  , withMaxAge
  , withReplicas
  , withDuplicateWindow
  , withAllowDirect
  , PurgeStreamOption
  , withPurgeSubject
  , withPurgeSequence
  , withPurgeKeep
  , StreamListOption
  , withStreamListOffset
  , withStreamListSubject
  , StreamInfo (..)
  , StreamState (..)
  , StreamCluster (..)
  , StreamPeer (..)
  , StreamSourceInfo (..)
  , DeleteStreamResponse (..)
  , PurgeStreamResponse (..)
  , StreamListResponse (..)
  , StreamNamesResponse (..)
  , durationToNanoseconds
  , nanosecondsToDuration
  ) where

import           JetStream.Error        (JetStreamError)
import           JetStream.Stream.Types
import           JetStream.Types        (StreamName, Subject)

-- | Stream management operations.
data StreamAPI = StreamAPI
                   { create :: StreamName -> [Subject] -> [StreamConfigOption] -> IO (Either JetStreamError StreamInfo)
                     -- ^ Create a stream.
                   , update :: StreamName -> [Subject] -> [StreamConfigOption] -> IO (Either JetStreamError StreamInfo)
                     -- ^ Update an existing stream configuration.
                   , info :: StreamName -> IO (Either JetStreamError StreamInfo)
                     -- ^ Fetch stream configuration and state.
                   , delete :: StreamName -> IO (Either JetStreamError DeleteStreamResponse)
                     -- ^ Delete a stream.
                   , purge :: StreamName -> [PurgeStreamOption] -> IO (Either JetStreamError PurgeStreamResponse)
                     -- ^ Purge messages from a stream.
                   , list :: [StreamListOption] -> IO (Either JetStreamError StreamListResponse)
                     -- ^ List streams and their metadata.
                   , names :: [StreamListOption] -> IO (Either JetStreamError StreamNamesResponse)
                   -- ^ List stream names.
                   }
