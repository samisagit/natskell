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

data MessageAPI = MessageAPI
                    { fetch :: StreamName -> ConsumerName -> PullRequest -> IO PullResponse
                    , ack :: Message -> IO (Either JetStreamError ())
                    , nak :: Message -> IO (Either JetStreamError ())
                    , inProgress :: Message -> IO (Either JetStreamError ())
                    , term :: Message -> IO (Either JetStreamError ())
                    }
