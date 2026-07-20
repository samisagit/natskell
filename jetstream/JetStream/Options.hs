{-# LANGUAGE OverloadedStrings #-}

module JetStream.Options
  ( JetStream (..)
  , JetStreamConfig (..)
  , JetStreamConfigError (..)
  , JetStreamContext (..)
  , JetStreamOption
  , JetStreamRequestOption
  , defaultJetStreamConfig
  , newJetStreamContext
  , tryNewJetStreamContext
  , requestTimeoutMicros
  , withDomain
  , withRequestTimeout
  , withRequestTimeoutMicros
  ) where

import qualified Client.API               as Nats
import qualified Data.ByteString          as BS
import           JetStream.Consumer.API   (ConsumerAPI)
import           JetStream.KeyValue.API   (KeyValueAPI)
import           JetStream.Management.API (ManagementAPI)
import           JetStream.Message.API    (MessageAPI)
import           JetStream.Publish.API    (PublishAPI)
import           JetStream.Stream.API     (StreamAPI)
import           JetStream.Types
    ( JetStreamRequestOption
    , applyRequestOptions
    , withRequestTimeout
    )

-- | JetStream capabilities. The constructor is kept in the internal package;
-- the public API exposes this type abstractly and exports its accessors.
data JetStream = JetStream
                   { streams    :: StreamAPI
                   , consumers  :: ConsumerAPI
                   , publisher  :: PublishAPI
                   , messages   :: MessageAPI
                   , management :: ManagementAPI
                   , keyValues  :: KeyValueAPI
                   }

data JetStreamConfig = JetStreamConfig
                         { configDomain               :: Maybe BS.ByteString
                         , configRequestTimeoutMicros :: Int
                         }
  deriving (Eq, Show)

data JetStreamContext = JetStreamContext
                          { contextClient               :: Nats.Client
                          , contextDomain               :: Maybe BS.ByteString
                          , contextRequestTimeoutMicros :: Int
                          }

newtype JetStreamOption = JetStreamOption (JetStreamConfig -> JetStreamConfig)

data JetStreamConfigError = EmptyJetStreamDomain
                          | InvalidJetStreamRequestTimeout Int
  deriving (Eq, Show)

defaultJetStreamConfig :: JetStreamConfig
defaultJetStreamConfig =
  JetStreamConfig
    { configDomain = Nothing
    , configRequestTimeoutMicros = 5 * 1000000
    }

newJetStreamContext :: Nats.Client -> [JetStreamOption] -> JetStreamContext
newJetStreamContext client options =
  let config = applyJetStreamOptions options defaultJetStreamConfig
  in JetStreamContext
       { contextClient = client
       , contextDomain = configDomain config
       , contextRequestTimeoutMicros = max 1 (configRequestTimeoutMicros config)
       }

tryNewJetStreamContext :: Nats.Client -> [JetStreamOption] -> Either JetStreamConfigError JetStreamContext
tryNewJetStreamContext client options =
  let config = applyJetStreamOptions options defaultJetStreamConfig
  in validateJetStreamConfig config >> pure (newJetStreamContext client options)

applyJetStreamOptions :: [JetStreamOption] -> JetStreamConfig -> JetStreamConfig
applyJetStreamOptions options config =
  foldl apply config options
  where
    apply value (JetStreamOption option) = option value

validateJetStreamConfig :: JetStreamConfig -> Either JetStreamConfigError ()
validateJetStreamConfig config
  | configDomain config == Just "" = Left EmptyJetStreamDomain
  | configRequestTimeoutMicros config <= 0 =
      Left (InvalidJetStreamRequestTimeout (configRequestTimeoutMicros config))
  | otherwise = Right ()

requestTimeoutMicros :: JetStreamContext -> [JetStreamRequestOption] -> Int
requestTimeoutMicros context =
  applyRequestOptions (contextRequestTimeoutMicros context)

withDomain :: BS.ByteString -> JetStreamOption
withDomain domain =
  JetStreamOption $ \config -> config { configDomain = Just domain }

withRequestTimeoutMicros :: Int -> JetStreamOption
withRequestTimeoutMicros timeoutMicros =
  JetStreamOption $ \config ->
    config { configRequestTimeoutMicros = timeoutMicros }
