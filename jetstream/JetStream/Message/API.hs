module JetStream.Message.API
  ( MessageAPI (..)
  , AckVerb (..)
  , Headers
  , Message (..)
  , PullRequest (..)
  , PullResponse (..)
  , PullStatus (..)
  , ackPayload
  , classifyStatusHeaders
  , defaultPullRequest
  , descriptionHeader
  , inProgressPayload
  , isStatusMessage
  , nakPayload
  , pullRequestPayload
  , statusHeader
  , termPayload
  ) where

import           JetStream.Error         (JetStreamError)
import           JetStream.Message.Types
    ( AckVerb (..)
    , Headers
    , Message (..)
    , PullRequest (..)
    , PullResponse (..)
    , PullStatus (..)
    , ackPayload
    , classifyStatusHeaders
    , defaultPullRequest
    , descriptionHeader
    , inProgressPayload
    , isStatusMessage
    , nakPayload
    , pullRequestPayload
    , statusHeader
    , termPayload
    )
import           JetStream.Types         (ConsumerName, StreamName)

-- | Pull-consumer message operations.
data MessageAPI = MessageAPI
                    { fetch :: StreamName -> ConsumerName -> PullRequest -> IO PullResponse
                      -- ^ Fetch messages for a pull consumer.
                    , ack :: Message -> IO (Either JetStreamError ())
                      -- ^ Acknowledge successful message processing.
                    , nak :: Message -> IO (Either JetStreamError ())
                      -- ^ Negatively acknowledge a message.
                    , inProgress :: Message -> IO (Either JetStreamError ())
                      -- ^ Tell the server that message processing is still in progress.
                    , term :: Message -> IO (Either JetStreamError ())
                    -- ^ Terminate message redelivery.
                    }
