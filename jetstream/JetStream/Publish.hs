module JetStream.Publish
  ( publishAPI
  , module JetStream.Publish.API
  ) where

import qualified Client.API                 as Nats
import qualified Data.ByteString            as BS
import           JetStream.Error            (JetStreamError (JetStreamNoReply))
import           JetStream.Options          (JetStreamContext)
import           JetStream.Protocol.Headers (statusError)
import           JetStream.Protocol.Request
    ( decodeJetStreamResponse
    , requestMsg
    )
import           JetStream.Publish.API
import           JetStream.Publish.Types    (PublishAPI (..), publishHeaders)
import           JetStream.Types
    ( JetStreamRequestOption
    , Payload
    , Subject
    )

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
  -> [JetStreamRequestOption]
  -> IO (Either JetStreamError PublishAck)
publishMessage context subject payload options requestOptions = do
  msgResult <- requestMsg context subject payload (publishHeaders options) requestOptions
  pure $ do
    msg <- msgResult
    case statusError msg of
      Just err ->
        Left err
      Nothing ->
        if BS.null (Nats.payload msg)
          then Left JetStreamNoReply
          else decodeJetStreamResponse (Nats.payload msg)
