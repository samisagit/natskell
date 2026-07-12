module JetStream.Message.API
  ( MessageAPI (..)
  , AckVerb (..)
  , FetchOption
  , FetchWait (..)
  , Headers
  , Message (..)
  , PullResponse (..)
  , PullStatus (..)
  , ackPayload
  , classifyStatusHeaders
  , descriptionHeader
  , inProgressPayload
  , isStatusMessage
  , nakPayload
  , statusHeader
  , termPayload
  , withFetchBatch
  , withFetchWait
  ) where

import           JetStream.Error         (JetStreamError)
import           JetStream.Message.Types
    ( AckVerb (..)
    , FetchOption
    , FetchWait (..)
    , Headers
    , Message (..)
    , PullResponse (..)
    , PullStatus (..)
    , ackPayload
    , classifyStatusHeaders
    , descriptionHeader
    , inProgressPayload
    , isStatusMessage
    , nakPayload
    , statusHeader
    , termPayload
    , withFetchBatch
    , withFetchWait
    )
import           JetStream.Types         (ConsumerName, StreamName)

-- | Pull-consumer message operations.
data MessageAPI = MessageAPI
                    { fetch :: StreamName -> ConsumerName -> [FetchOption] -> IO PullResponse
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
