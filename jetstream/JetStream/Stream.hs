module JetStream.Stream
  ( streamAPI
  , module JetStream.Stream.API
  ) where

import           Data.Aeson                 (toJSON)
import           JetStream.Error
    ( JetStreamApiError (..)
    , JetStreamError (..)
    )
import           JetStream.Options          (JetStreamContext)
import qualified JetStream.Protocol.Request as Request
import qualified JetStream.Protocol.Subject as Subject
import           JetStream.Stream.API
import           JetStream.Stream.Types
    ( StreamAPI (..)
    , StreamConfigRequest
    , purgeStreamRequest
    , streamConfigRequest
    , streamListRequest
    , streamMessageDeleteRequest
    , streamMessageGetRequest
    , streamNamesRequest
    , validateStreamConfigRequest
    )
import           JetStream.Types            (JetStreamRequestOption, Subject)

streamAPI :: JetStreamContext -> StreamAPI
streamAPI context =
  StreamAPI
    { create = \name subjects options requestOptions ->
        let config = streamConfigRequest name subjects options
        in requestStreamConfig context
          (Subject.streamCreateSubject context name)
          config
          requestOptions
    , createOrUpdate = \name subjects options requestOptions -> do
        updateResult <- update (streamAPI context) name subjects options requestOptions
        case updateResult of
          Left err
            | isStreamNotFound err ->
                create (streamAPI context) name subjects options requestOptions
          other ->
            pure other
    , update = \name subjects options requestOptions ->
        let config = streamConfigRequest name subjects options
        in requestStreamConfig context
          (Subject.streamUpdateSubject context name)
          config
          requestOptions
    , info = \streamName requestOptions ->
        Request.requestJSON context
          (Subject.streamInfoSubject context streamName)
          Nothing
          requestOptions
    , getMessage = \streamName selector requestOptions ->
        Request.requestJSON context
          (Subject.streamMessageGetSubject context streamName)
          (Just (toJSON (streamMessageGetRequest selector)))
          requestOptions
    , deleteMessage = \streamName sequenceNumber mode requestOptions ->
        Request.requestJSON context
          (Subject.streamMessageDeleteSubject context streamName)
          (Just (toJSON (streamMessageDeleteRequest sequenceNumber mode)))
          requestOptions
    , delete = \streamName requestOptions ->
        Request.requestJSON context
          (Subject.streamDeleteSubject context streamName)
          Nothing
          requestOptions
    , purge = \streamName options requestOptions ->
        Request.requestJSON context
          (Subject.streamPurgeSubject context streamName)
          (Just (toJSON (purgeStreamRequest options)))
          requestOptions
    , list =
        Request.requestJSON context (Subject.streamListSubject context)
          . Just . toJSON . streamListRequest
    , names =
        Request.requestJSON context (Subject.streamNamesSubject context)
          . Just . toJSON . streamNamesRequest
    }

requestStreamConfig
  :: JetStreamContext
  -> Subject
  -> StreamConfigRequest
  -> [JetStreamRequestOption]
  -> IO (Either JetStreamError StreamInfo)
requestStreamConfig context subject config requestOptions =
  case validateStreamConfigRequest config of
    Left err ->
      pure (Left err)
    Right () ->
      Request.requestJSON context subject (Just (toJSON config)) requestOptions

isStreamNotFound :: JetStreamError -> Bool
isStreamNotFound (JetStreamApiFailure err) =
  apiErrorCodeDetail err == 10059
isStreamNotFound _ =
  False
