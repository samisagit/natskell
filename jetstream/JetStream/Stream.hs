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
import           JetStream.Stream.API       (StreamAPI (..))
import           JetStream.Stream.Types
    ( purgeStreamRequest
    , streamConfigRequest
    , streamListRequest
    , streamMessageDeleteRequest
    , streamMessageGetRequest
    , streamNamesRequest
    )

streamAPI :: JetStreamContext -> StreamAPI
streamAPI context =
  StreamAPI
    { create = \name subjects options ->
        let config = streamConfigRequest name subjects options
        in Request.requestJSON context
          (Subject.streamCreateSubject context name)
          (Just (toJSON config))
    , createOrUpdate = \name subjects options -> do
        updateResult <- update (streamAPI context) name subjects options
        case updateResult of
          Left err
            | isStreamNotFound err ->
                create (streamAPI context) name subjects options
          other ->
            pure other
    , update = \name subjects options ->
        let config = streamConfigRequest name subjects options
        in Request.requestJSON context
          (Subject.streamUpdateSubject context name)
          (Just (toJSON config))
    , info = \streamName ->
        Request.requestJSON context
          (Subject.streamInfoSubject context streamName)
          Nothing
    , getMessage = \streamName selector ->
        Request.requestJSON context
          (Subject.streamMessageGetSubject context streamName)
          (Just (toJSON (streamMessageGetRequest selector)))
    , deleteMessage = \streamName sequenceNumber mode ->
        Request.requestJSON context
          (Subject.streamMessageDeleteSubject context streamName)
          (Just (toJSON (streamMessageDeleteRequest sequenceNumber mode)))
    , delete = \streamName ->
        Request.requestJSON context
          (Subject.streamDeleteSubject context streamName)
          Nothing
    , purge = \streamName options ->
        Request.requestJSON context
          (Subject.streamPurgeSubject context streamName)
          (Just (toJSON (purgeStreamRequest options)))
    , list =
        Request.requestJSON context
          (Subject.streamListSubject context) .
          Just . toJSON . streamListRequest
    , names =
        Request.requestJSON context
          (Subject.streamNamesSubject context) .
          Just . toJSON . streamNamesRequest
    }

isStreamNotFound :: JetStreamError -> Bool
isStreamNotFound (JetStreamApiFailure err) =
  apiErrorCodeDetail err == 10059
isStreamNotFound _ =
  False
