module JetStream.Stream
  ( streamAPI
  , module JetStream.Stream.API
  ) where

import           Data.Aeson                 (toJSON)
import           JetStream.Options          (JetStreamContext)
import qualified JetStream.Protocol.Request as Request
import qualified JetStream.Protocol.Subject as Subject
import           JetStream.Stream.API

streamAPI :: JetStreamContext -> StreamAPI
streamAPI context =
  StreamAPI
    { create = \config ->
        Request.requestJSON context
          (Subject.streamCreateSubject context (streamConfigName config))
          (Just (toJSON config))
    , update = \config ->
        Request.requestJSON context
          (Subject.streamUpdateSubject context (streamConfigName config))
          (Just (toJSON config))
    , info = \streamName ->
        Request.requestJSON context
          (Subject.streamInfoSubject context streamName)
          Nothing
    , delete = \streamName ->
        Request.requestJSON context
          (Subject.streamDeleteSubject context streamName)
          Nothing
    , purge = \streamName request ->
        Request.requestJSON context
          (Subject.streamPurgeSubject context streamName)
          (Just (toJSON request))
    , list =
        Request.requestJSON context
          (Subject.streamListSubject context)
          . Just . toJSON
    , names =
        Request.requestJSON context
          (Subject.streamNamesSubject context)
          . Just . toJSON
    }
