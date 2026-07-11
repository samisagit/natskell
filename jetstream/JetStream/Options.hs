module JetStream.Options
  ( JetStreamConfig (..)
  , JetStreamContext (..)
  , JetStreamOption
  , defaultJetStreamConfig
  , newJetStreamContext
  , withDomain
  , withRequestTimeoutMicros
  ) where

import qualified API             as Nats
import qualified Data.ByteString as BS

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

type JetStreamOption = JetStreamConfig -> JetStreamConfig

defaultJetStreamConfig :: JetStreamConfig
defaultJetStreamConfig =
  JetStreamConfig
    { configDomain = Nothing
    , configRequestTimeoutMicros = 5 * 1000000
    }

newJetStreamContext :: Nats.Client -> [JetStreamOption] -> JetStreamContext
newJetStreamContext client options =
  let config = foldl (flip ($)) defaultJetStreamConfig options
  in JetStreamContext
    { contextClient = client
    , contextDomain = configDomain config
    , contextRequestTimeoutMicros = configRequestTimeoutMicros config
    }

withDomain :: BS.ByteString -> JetStreamOption
withDomain domain config =
  config { configDomain = Just domain }

withRequestTimeoutMicros :: Int -> JetStreamOption
withRequestTimeoutMicros timeoutMicros config =
  config { configRequestTimeoutMicros = max 1 timeoutMicros }
