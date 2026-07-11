module JetStream.Stream.API
  ( StreamAPI (..)
  , module JetStream.Stream.Types
  ) where

import           JetStream.Error        (JetStreamError)
import           JetStream.Stream.Types
import           JetStream.Types        (StreamName)

data StreamAPI = StreamAPI
                   { create :: StreamConfig -> IO (Either JetStreamError StreamInfo)
                   , update :: StreamConfig -> IO (Either JetStreamError StreamInfo)
                   , info :: StreamName -> IO (Either JetStreamError StreamInfo)
                   , delete :: StreamName -> IO (Either JetStreamError DeleteStreamResponse)
                   , purge :: StreamName -> PurgeStreamRequest -> IO (Either JetStreamError PurgeStreamResponse)
                   , list :: StreamListRequest -> IO (Either JetStreamError StreamListResponse)
                   , names :: StreamNamesRequest -> IO (Either JetStreamError StreamNamesResponse)
                   }

