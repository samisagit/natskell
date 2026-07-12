{-# LANGUAGE OverloadedStrings #-}

module JetStream.Consumer
  ( consumerAPI
  , module JetStream.Consumer.API
  ) where

import           Data.Aeson                 (Value, object, toJSON, (.=))
import           Data.Maybe                 (catMaybes)
import           JetStream.Consumer.API     (ConsumerAPI (..))
import           JetStream.Consumer.Types
    ( ConsumerConfigAction (..)
    , ConsumerConfigOption
    , ConsumerConfigRequest
    , consumerConfigAction
    , consumerConfigRequest
    , consumerListRequest
    , consumerNamesRequest
    , consumerPauseRequest
    , consumerResetRequest
    , ensureDurableConsumerConfig
    , ensureNamedConsumerConfig
    , withConsumerDeliverSubject
    )
import           JetStream.Options          (JetStreamContext)
import qualified JetStream.Protocol.Request as Request
import qualified JetStream.Protocol.Subject as Subject
import           JetStream.Types
    ( ConsumerName
    , StreamName
    , Subject
    , byteStringToJSON
    )

consumerAPI :: JetStreamContext -> ConsumerAPI
consumerAPI context =
  ConsumerAPI
    { createConsumer = \stream options ->
        Request.requestJSON context
          (Subject.consumerCreateSubject context stream)
          (Just (createConsumerRequest stream Nothing (consumerConfigRequest options)))
    , createOrUpdateConsumer = \stream consumer options ->
        let config = ensureNamedConsumerConfig consumer (consumerConfigRequest options)
        in Request.requestJSON context
          (Subject.consumerCreateNamedSubject context stream consumer)
          (Just (createConsumerRequest stream (consumerConfigAction ConsumerCreateOrUpdate) config))
    , updateConsumer = \stream consumer options ->
        let config = ensureNamedConsumerConfig consumer (consumerConfigRequest options)
        in Request.requestJSON context
          (Subject.consumerCreateNamedSubject context stream consumer)
          (Just (createConsumerRequest stream (consumerConfigAction ConsumerUpdate) config))
    , createDurableConsumer = \stream durable options ->
        let config = ensureDurableConsumerConfig durable (consumerConfigRequest options)
        in Request.requestJSON context
          (Subject.durableConsumerCreateSubject context stream durable)
          (Just (createConsumerRequest stream Nothing config))
    , createOrUpdateDurableConsumer = \stream durable options ->
        let config = ensureDurableConsumerConfig durable (consumerConfigRequest options)
        in Request.requestJSON context
          (Subject.consumerCreateNamedSubject context stream durable)
          (Just (createConsumerRequest stream (consumerConfigAction ConsumerCreateOrUpdate) config))
    , updateDurableConsumer = \stream durable options ->
        let config = ensureDurableConsumerConfig durable (consumerConfigRequest options)
        in Request.requestJSON context
          (Subject.consumerCreateNamedSubject context stream durable)
          (Just (createConsumerRequest stream (consumerConfigAction ConsumerUpdate) config))
    , createPushConsumer = \stream consumer deliverSubject options ->
        let config = pushConsumerConfig consumer deliverSubject options
        in Request.requestJSON context
          (Subject.consumerCreateNamedSubject context stream consumer)
          (Just (createConsumerRequest stream (consumerConfigAction ConsumerCreate) config))
    , createOrUpdatePushConsumer = \stream consumer deliverSubject options ->
        let config = pushConsumerConfig consumer deliverSubject options
        in Request.requestJSON context
          (Subject.consumerCreateNamedSubject context stream consumer)
          (Just (createConsumerRequest stream (consumerConfigAction ConsumerCreateOrUpdate) config))
    , updatePushConsumer = \stream consumer deliverSubject options ->
        let config = pushConsumerConfig consumer deliverSubject options
        in Request.requestJSON context
          (Subject.consumerCreateNamedSubject context stream consumer)
          (Just (createConsumerRequest stream (consumerConfigAction ConsumerUpdate) config))
    , consumerInfo = \stream consumer ->
        Request.requestJSON context
          (Subject.consumerInfoSubject context stream consumer)
          Nothing
    , pauseConsumer = \stream consumer pauseUntil ->
        Request.requestJSON context
          (Subject.consumerPauseSubject context stream consumer)
          (Just (toJSON (consumerPauseRequest (Just pauseUntil))))
    , resumeConsumer = \stream consumer ->
        Request.requestJSON context
          (Subject.consumerPauseSubject context stream consumer)
          (Just (toJSON (consumerPauseRequest Nothing)))
    , resetConsumer = \stream consumer options ->
        Request.requestJSON context
          (Subject.consumerResetSubject context stream consumer)
          (Just (toJSON (consumerResetRequest options)))
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

pushConsumerConfig :: ConsumerName -> Subject -> [ConsumerConfigOption] -> ConsumerConfigRequest
pushConsumerConfig consumer deliverSubject options =
  ensureNamedConsumerConfig consumer $
    consumerConfigRequest (withConsumerDeliverSubject deliverSubject : options)

createConsumerRequest :: StreamName -> Maybe StreamName -> ConsumerConfigRequest -> Value
createConsumerRequest stream action config =
  object . catMaybes $
    [ Just ("stream_name" .= byteStringToJSON stream)
    , Just ("config" .= toJSON config)
    , fmap (("action" .=) . byteStringToJSON) action
    ]
