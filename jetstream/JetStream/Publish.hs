module JetStream.Publish
  ( publishAPI
  , module JetStream.Publish.API
  ) where

import qualified Client.API                 as Nats
import           JetStream.Error            (JetStreamError (JetStreamNoReply))
import           JetStream.Options          (JetStreamContext)
import           JetStream.Protocol.Headers (statusError)
import           JetStream.Protocol.Request
    ( decodeJetStreamResponse
    , requestMsg
    )
import           JetStream.Publish.API
import           JetStream.Publish.Types    (publishHeaders)
import           JetStream.Types            (Payload, Subject)

publishAPI :: JetStreamContext -> PublishAPI
publishAPI context =
  PublishAPI
    { publish = publishMessage context
    }

publishMessage
  :: JetStreamContext
  -> Subject
  -> Payload
  -> [PublishOption]
  -> IO (Either JetStreamError PublishAck)
publishMessage context subject payload options = do
  msgResult <- requestMsg context subject (Just payload) (publishHeaders options)
  pure $ do
    msg <- msgResult
    case statusError msg of
      Just err ->
        Left err
      Nothing ->
        case Nats.payload msg of
          Nothing ->
            Left JetStreamNoReply
          Just ackPayload ->
            decodeJetStreamResponse ackPayload
