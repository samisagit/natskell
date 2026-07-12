{-# LANGUAGE OverloadedStrings #-}

module JetStream.Consumer
  ( consumerAPI
  , module JetStream.Consumer.API
  ) where

import           Data.Aeson                 (Value, object, toJSON, (.=))
import           JetStream.Consumer.API     (ConsumerAPI (..))
import           JetStream.Consumer.Types
    ( ConsumerConfigRequest
    , consumerConfigRequest
    , consumerListRequest
    , consumerNamesRequest
    , ensureDurableConsumerConfig
    )
import           JetStream.Options          (JetStreamContext)
import qualified JetStream.Protocol.Request as Request
import qualified JetStream.Protocol.Subject as Subject
import           JetStream.Types            (StreamName, byteStringToJSON)

consumerAPI :: JetStreamContext -> ConsumerAPI
consumerAPI context =
  ConsumerAPI
    { createConsumer = \stream options ->
        Request.requestJSON context
          (Subject.consumerCreateSubject context stream)
          (Just (createConsumerRequest stream (consumerConfigRequest options)))
    , createDurableConsumer = \stream durable options ->
        let config = ensureDurableConsumerConfig durable (consumerConfigRequest options)
        in Request.requestJSON context
          (Subject.durableConsumerCreateSubject context stream durable)
          (Just (createConsumerRequest stream config))
    , consumerInfo = \stream consumer ->
        Request.requestJSON context
          (Subject.consumerInfoSubject context stream consumer)
          Nothing
    , deleteConsumer = \stream consumer ->
        Request.requestJSON context
          (Subject.consumerDeleteSubject context stream consumer)
          Nothing
    , listConsumers = \stream options ->
        Request.requestJSON context
          (Subject.consumerListSubject context stream)
          (Just (toJSON (consumerListRequest options)))
    , consumerNames = \stream options ->
        Request.requestJSON context
          (Subject.consumerNamesSubject context stream)
          (Just (toJSON (consumerNamesRequest options)))
    }

createConsumerRequest :: StreamName -> ConsumerConfigRequest -> Value
createConsumerRequest stream config =
  object
    [ "stream_name" .= byteStringToJSON stream
    , "config" .= toJSON config
    ]
