-- | Internal home for the core NATS client capability surface.
--
-- The public @API@ module deliberately hides the constructors defined here.
-- Keeping construction private lets the capability grow without changing the
-- public representation.
module Client.API
  ( Client (..)
  , Message (..)
  , MsgView
  , Subscription (..)
  , subscriptionSid
  , NatsError (..)
  , ConnectionState (..)
  , Subject
  , Payload
  , Headers
  , PublishOption
  , SubscribeOption
  , RequestOption
  , UnsubscribeOption
  , PingOption
  , FlushOption
  , ResetOption
  , CloseOption
  , RequestConfig (..)
  , UnsubscribeConfig (..)
  , PingConfig (..)
  , FlushConfig (..)
  , ResetConfig (..)
  , CloseConfig (..)
  , withSubscriptionExpiry
  , withQueueGroup
  , withReplyTo
  , withHeaders
  , withRequestTimeout
  , withRequestHeaders
  , withPingTimeout
  , withFlushTimeout
  ) where

import qualified Data.ByteString    as BS
import           Data.Time.Clock    (NominalDiffTime)
import           Lib.CallOption     (CallOption)
import           Publish.Config     (PublishConfig (..))
import           State.Types        (ClientExitReason, ConnectionState (..))
import           Subscription.Types (SubscribeConfig (..))
import           Types.Msg          (Headers, Payload, SID, Subject)

-- | Client capabilities for publishing, subscribing, request-reply, and
-- lifecycle control.
--
-- This constructor is internal. Public callers receive a 'Client' from
-- 'Client.newClient' and use the named operations re-exported by @API@.
data Client = Client
                { publish :: Subject -> Payload -> [PublishOption] -> IO (Either NatsError ())
                  -- ^ Publish a message.
                , subscribe :: Subject -> [SubscribeOption] -> (Message -> IO ()) -> IO (Either NatsError Subscription)
                  -- ^ Subscribe to a subject until explicitly unsubscribed.
                , subscribeOnce :: Subject -> [SubscribeOption] -> (Message -> IO ()) -> IO (Either NatsError Subscription)
                  -- ^ Subscribe until the first message is delivered.
                , request :: Subject -> Payload -> [RequestOption] -> IO (Either NatsError Message)
                  -- ^ Publish a request and wait for one response.
                , unsubscribe :: Subscription -> [UnsubscribeOption] -> IO (Either NatsError ())
                  -- ^ Unsubscribe a subscription created by this client.
                , newInbox :: IO Subject
                  -- ^ Create a unique inbox subject for replies.
                , ping :: [PingOption] -> IO (Either NatsError ())
                  -- ^ Round trip a PING through the server.
                , flush :: [FlushOption] -> IO (Either NatsError ())
                  -- ^ Wait until all previously buffered writes reach the server.
                , connectionState :: IO ConnectionState
                  -- ^ Read the client's current connection lifecycle state.
                , reset :: [ResetOption] -> IO ()
                  -- ^ Reset the client connection state.
                , close :: [CloseOption] -> IO ()
                -- ^ Close the client connection and release resources.
                }

-- | A message delivered by NATS.
--
-- NATS always has a payload, which may be empty. This is intentionally
-- represented by a strict 'BS.ByteString', not @Maybe ByteString@.
data Message = Message
                 { subject :: Subject
                 , sid     :: SID
                 , replyTo :: Maybe Subject
                 , payload :: Payload
                 , headers :: Maybe Headers
                 }
  deriving (Eq, Show)

-- | Compatibility name retained for code written against pre-0.4 releases.
type MsgView = Message

-- | An opaque handle to an active subscription.
newtype Subscription = Subscription SID
  deriving (Eq, Show)

-- | The protocol identifier used by the legacy @API@ module. New code should
-- treat 'Subscription' as an opaque handle.
subscriptionSid :: Subscription -> SID
subscriptionSid (Subscription value) = value

-- | Failures reported by core NATS operations.
data NatsError = NatsValidationError BS.ByteString
               | NatsPayloadTooLarge Int Int
               | NatsSlowConsumer
               | NatsConnectionClosed ClientExitReason
               | NatsRequestTimedOut
               | NatsNoResponders
  deriving (Eq, Show)

type PublishOption = CallOption PublishConfig

type SubscribeOption = CallOption SubscribeConfig

data RequestConfig = RequestConfig
                       { requestTimeout :: NominalDiffTime
                       , requestHeaders :: Maybe Headers
                       }

type RequestOption = CallOption RequestConfig

data UnsubscribeConfig = UnsubscribeConfig
type UnsubscribeOption = CallOption UnsubscribeConfig

-- Internal configuration constructors are not covered by public API stability.
data PingConfig = PingConfig
                | PingConfigTimeout NominalDiffTime
type PingOption = CallOption PingConfig

data FlushConfig = FlushConfig
                 | FlushConfigTimeout NominalDiffTime
type FlushOption = CallOption FlushConfig

data ResetConfig = ResetConfig
type ResetOption = CallOption ResetConfig

data CloseConfig = CloseConfig
type CloseOption = CallOption CloseConfig

-- | Stop a one-shot subscription if it has not received a message within the
-- given interval.
withSubscriptionExpiry :: NominalDiffTime -> SubscribeOption
withSubscriptionExpiry expirySeconds cfg = cfg { expiry = Just expirySeconds }

-- | Deliver messages to one member of the named queue group.
withQueueGroup :: Subject -> SubscribeOption
withQueueGroup queueGroup cfg = cfg { subscribeQueueGroup = Just queueGroup }

-- | Set the reply subject on an outgoing publish.
withReplyTo :: Subject -> PublishOption
withReplyTo replySubject cfg = cfg { publishReplyTo = Just replySubject }

-- | Set headers on an outgoing publish.
withHeaders :: Headers -> PublishOption
withHeaders messageHeaders cfg = cfg { publishHeaders = Just messageHeaders }

-- | Set the amount of time to wait for a request reply.
withRequestTimeout :: NominalDiffTime -> RequestOption
withRequestTimeout timeout cfg = cfg { requestTimeout = max 0 timeout }

-- | Set headers on an outgoing request.
withRequestHeaders :: Headers -> RequestOption
withRequestHeaders messageHeaders cfg = cfg { requestHeaders = Just messageHeaders }

-- | Set the maximum time to wait for a ping response.
withPingTimeout :: NominalDiffTime -> PingOption
withPingTimeout timeout _ = PingConfigTimeout (max 0 timeout)

-- | Set the maximum time to wait for a flush response.
withFlushTimeout :: NominalDiffTime -> FlushOption
withFlushTimeout timeout _ = FlushConfigTimeout (max 0 timeout)
