module JetStream.Stream.API
  ( StreamAPI
  , create
  , createOrUpdate
  , update
  , info
  , getMessage
  , deleteMessage
  , delete
  , purge
  , list
  , names
  , Stream
  , streamName
  , lookupStream
  , createStream
  , getStreamInfo
  , RetentionPolicy (..)
  , StorageType (..)
  , DiscardPolicy (..)
  , StreamConfig
  , streamConfigName
  , streamConfigSubjects
  , streamConfigRetention
  , streamConfigStorage
  , streamConfigDiscard
  , streamConfigMaxMessages
  , streamConfigMaxBytes
  , streamConfigMaxAge
  , streamConfigReplicas
  , streamConfigDuplicateWindow
  , streamConfigAllowDirect
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
  , StreamMessage
  , streamMessageSubject
  , streamMessageSequence
  , streamMessageHeadersRaw
  , streamMessagePayload
  , streamMessageTime
  , StreamMessageSelector (..)
  , StreamMessageDeleteMode (..)
  , DeleteStreamMessageResponse
  , deleteStreamMessageSuccess
  , StreamInfo
  , streamInfoConfig
  , streamInfoCreated
  , streamInfoState
  , streamInfoCluster
  , streamInfoMirror
  , streamInfoSources
  , StreamState
  , streamStateMessages
  , streamStateBytes
  , streamStateFirstSequence
  , streamStateFirstTime
  , streamStateLastSequence
  , streamStateLastTime
  , streamStateConsumerCount
  , streamStateDeleted
  , streamStateNumDeleted
  , streamStateNumSubjects
  , StreamCluster
  , streamClusterName
  , streamClusterLeader
  , streamClusterReplicas
  , StreamPeer
  , streamPeerName
  , streamPeerCurrent
  , streamPeerOffline
  , streamPeerActive
  , streamPeerLag
  , StreamSourceInfo
  , streamSourceInfoName
  , streamSourceInfoFilterSubject
  , streamSourceInfoLag
  , streamSourceInfoActive
  , DeleteStreamResponse
  , deleteStreamSuccess
  , PurgeStreamResponse
  , purgeStreamSuccess
  , purgeStreamPurged
  , StreamListResponse
  , streamListTotal
  , streamListOffset
  , streamListLimit
  , streamListStreams
  , StreamNamesResponse
  , streamNamesTotal
  , streamNamesOffset
  , streamNamesLimit
  , streamNamesStreams
  ) where

import           JetStream.Error        (JetStreamError)
import           JetStream.Stream.Types
import           JetStream.Types
    ( JetStreamRequestOption
    , StreamName
    , Subject
    )

-- | Look up a stream and return a stable resource handle.
lookupStream
  :: StreamAPI
  -> StreamName
  -> [JetStreamRequestOption]
  -> IO (Either JetStreamError Stream)
lookupStream api name requestOptions =
  fmap (fmap (const (Stream name))) (info api name requestOptions)

-- | Create a stream and return a stable resource handle.
createStream
  :: StreamAPI
  -> StreamName
  -> [Subject]
  -> [StreamConfigOption]
  -> [JetStreamRequestOption]
  -> IO (Either JetStreamError Stream)
createStream api name subjects configOptions requestOptions =
  fmap (fmap (const (Stream name)))
    (create api name subjects configOptions requestOptions)

-- | Read the current info for a stream handle.
getStreamInfo
  :: StreamAPI
  -> Stream
  -> [JetStreamRequestOption]
  -> IO (Either JetStreamError StreamInfo)
getStreamInfo api stream =
  info api (streamName stream)
