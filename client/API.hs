-- | Capability record for the NATS client surface.
module API
  (
    Client (..)
  , MsgView (..)
  , PublishOption
  , SubscribeOption
  , withSubscriptionExpiry
  , withPayload
  , withReplyCallback
  , withHeaders
  ) where

import           Client.PublishAPI      (PublishConfig)
import           Client.SubscriptionAPI (SubscribeConfig (..))
import qualified Data.ByteString        as BS
import           Data.Time.Clock        (NominalDiffTime)
import           Lib.CallOption         (CallOption)
import           Types                  (Headers, Payload, SID, Subject)
import qualified Types.Msg              as Msg

-- | Client capabilities for publishing, subscribing, and lifecycle control.
data Client = Client
                { publish :: Subject -> [PublishOption] -> IO ()
                  -- ^ Publish a message, optionally overriding publish options.
                , subscribe :: Subject -> [SubscribeOption] -> (Maybe MsgView -> IO ()) -> IO SID
                  -- ^ Subscribe to a subject and handle delivered messages.
                , request :: Subject -> [SubscribeOption] -> (Maybe MsgView -> IO ()) -> IO SID
                  -- ^ Subscribe with request semantics and auto-unsubscribe after a reply.
                , unsubscribe :: SID -> IO ()
                  -- ^ Unsubscribe from a subscription by SID.
                , ping :: IO () -> IO ()
                  -- ^ Send a ping and run the callback when a pong arrives.
                , flush :: IO ()
                  -- ^ Flush buffered writes to the server.
                , reset :: IO ()
                  -- ^ Reset the client connection state.
                , close :: IO ()
                -- ^ Close the client connection and release resources.
                }

-- | MsgView represents a MSG in the NATS protocol.
data MsgView = MsgView
                 { -- | The subject of the message.
                   subject :: BS.ByteString
                   -- | The SID (subscription ID) of the message.
                 , sid     :: BS.ByteString
                   -- | The replyTo subject, if any.
                 , replyTo :: Maybe BS.ByteString
                   -- | The payload of the message, if any.
                 , payload :: Maybe BS.ByteString
                   -- | Headers associated with the message, if any.
                 , headers :: Maybe [(BS.ByteString, BS.ByteString)]
                 }
  deriving (Eq, Show)

type PublishOption = CallOption PublishConfig

type SubscribeOption = CallOption SubscribeConfig

-- | withSubscriptionExpiry sets the reply subscription expiry in seconds.
-- Default: no expiry (reply subscriptions stay open until unsubscribe).
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- subscribe client \"events.created\" [withSubscriptionExpiry 2] print
-- @
withSubscriptionExpiry :: NominalDiffTime -> SubscribeOption
withSubscriptionExpiry expirySeconds cfg = cfg { subscriptionExpiry = Just expirySeconds }

-- | withPayload is used to set the payload for a publish operation.
-- Default: no payload.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- publish client \"updates\" [withPayload \"hello\"]
-- @
withPayload :: Payload -> PublishOption
withPayload payload (_, callback, headers) = (Just payload, callback, headers)

-- | withReplyCallback is used to set a callback for a reply to a publish operation.
-- Default: no reply subscription; publishes are fire-and-forget.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- publish client \"service.echo\" [withReplyCallback print]
-- @
withReplyCallback :: (Maybe MsgView -> IO ()) -> PublishOption
withReplyCallback callback (payload, _, headers) =
  (payload, Just (callback . fmap (\msg -> MsgView
    { subject = Msg.subject msg
    , sid = Msg.sid msg
    , replyTo = Msg.replyTo msg
    , payload = Msg.payload msg
    , headers = Msg.headers msg
    })), headers)

-- | withHeaders is used to set headers for a publish operation.
-- Default: no headers.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- publish client \"updates\" [withHeaders [(\"source\", \"test\")]]
-- @
withHeaders :: Headers -> PublishOption
withHeaders headers (payload, callback, _) = (payload, callback, Just headers)
