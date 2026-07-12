module JetStream.Stream
  ( streamAPI
  , module JetStream.Stream.API
  ) where

import           Data.Aeson                 (toJSON)
import           JetStream.Options          (JetStreamContext)
import qualified JetStream.Protocol.Request as Request
import qualified JetStream.Protocol.Subject as Subject
import           JetStream.Stream.API       (StreamAPI (..))
import           JetStream.Stream.Types
    ( purgeStreamRequest
    , streamConfigRequest
    , streamListRequest
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
    , update = \name subjects options ->
        let config = streamConfigRequest name subjects options
        in Request.requestJSON context
          (Subject.streamUpdateSubject context name)
          (Just (toJSON config))
    , info = \streamName ->
        Request.requestJSON context
          (Subject.streamInfoSubject context streamName)
          Nothing
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
