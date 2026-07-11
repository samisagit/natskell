{-# LANGUAGE OverloadedStrings #-}

module JetStream.Consumer
  ( consumerAPI
  , module JetStream.Consumer.API
  ) where

import           Data.Aeson                 (Value, object, toJSON, (.=))
import qualified Data.ByteString            as BS
import           JetStream.Consumer.API
import           JetStream.Options          (JetStreamContext)
import qualified JetStream.Protocol.Request as Request
import qualified JetStream.Protocol.Subject as Subject
import           JetStream.Types
    ( ConsumerName
    , StreamName
    , byteStringToJSON
    )

consumerAPI :: JetStreamContext -> ConsumerAPI
consumerAPI context =
  ConsumerAPI
    { createConsumer = \stream config ->
        Request.requestJSON context
          (Subject.consumerCreateSubject context stream)
          (Just (createConsumerRequest stream config))
    , createDurableConsumer = \stream durable config ->
        Request.requestJSON context
          (Subject.durableConsumerCreateSubject context stream durable)
          (Just (createConsumerRequest stream (withDurable durable config)))
    , consumerInfo = \stream consumer ->
        Request.requestJSON context
          (Subject.consumerInfoSubject context stream consumer)
          Nothing
    , deleteConsumer = \stream consumer ->
        Request.requestJSON context
          (Subject.consumerDeleteSubject context stream consumer)
          Nothing
    , listConsumers = \stream request ->
        Request.requestJSON context
          (Subject.consumerListSubject context stream)
          (Just (toJSON request))
    , consumerNames = \stream request ->
        Request.requestJSON context
          (Subject.consumerNamesSubject context stream)
          (Just (toJSON request))
    }

withDurable :: ConsumerName -> ConsumerConfig -> ConsumerConfig
withDurable durable config =
  config
    { consumerConfigDurableName = choose (consumerConfigDurableName config)
    , consumerConfigName = choose (consumerConfigName config)
    }
  where
    choose Nothing = Just durable
    choose (Just existing)
      | BS.null existing = Just durable
      | otherwise = Just existing

createConsumerRequest :: StreamName -> ConsumerConfig -> Value
createConsumerRequest stream config =
  object
    [ "stream_name" .= byteStringToJSON stream
    , "config" .= toJSON config
    ]
