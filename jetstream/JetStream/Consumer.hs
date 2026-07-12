{-# LANGUAGE OverloadedStrings #-}

module JetStream.Consumer
  ( consumerAPI
  , module JetStream.Consumer.API
  ) where

import           Data.Aeson                 (Value, object, toJSON, (.=))
import           Data.Maybe                 (catMaybes)
import           JetStream.Consumer.API     (ConsumerAPI (..))
import           JetStream.Consumer.Types
    ( ConsumerAction (..)
    , ConsumerConfigOption
    , ConsumerConfigRequest
    , ConsumerKind
    , ConsumerTarget (..)
    , applyConsumerKind
    , applyConsumerTarget
    , consumerActionValue
    , consumerConfigRequest
    , consumerListRequest
    , consumerNamesRequest
    , consumerPauseRequest
    , consumerResetRequest
    )
import           JetStream.Error
    ( JetStreamError (JetStreamDecodeError)
    )
import           JetStream.Options          (JetStreamContext)
import qualified JetStream.Protocol.Request as Request
import qualified JetStream.Protocol.Subject as Subject
import           JetStream.Types
    ( StreamName
    , Subject
    , byteStringToJSON
    )

consumerAPI :: JetStreamContext -> ConsumerAPI
consumerAPI context =
  ConsumerAPI
    { putConsumer = \stream action target kind options ->
        case consumerRequest context stream action target kind options of
          Left err ->
            pure (Left err)
          Right (subject, actionValue, config) ->
            Request.requestJSON context
              subject
              (Just (createConsumerRequest stream actionValue config))
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

consumerRequest
  :: JetStreamContext
  -> StreamName
  -> ConsumerAction
  -> ConsumerTarget
  -> ConsumerKind
  -> [ConsumerConfigOption]
  -> Either JetStreamError (Subject, Maybe StreamName, ConsumerConfigRequest)
consumerRequest context stream action target kind options =
  case consumerSubject context stream action target of
    Left err ->
      Left err
    Right subject ->
      Right
        ( subject
        , actionValue
        , applyConsumerTarget target . applyConsumerKind kind $ consumerConfigRequest options
        )
  where
    actionValue =
      case target of
        EphemeralConsumer ->
          Nothing
        DurableConsumer _ | action == ConsumerCreate ->
          Nothing
        _ ->
          consumerActionValue action

consumerSubject
  :: JetStreamContext
  -> StreamName
  -> ConsumerAction
  -> ConsumerTarget
  -> Either JetStreamError Subject
consumerSubject context stream ConsumerCreate EphemeralConsumer =
  Right (Subject.consumerCreateSubject context stream)
consumerSubject context stream ConsumerCreate (DurableConsumer durable) =
  Right (Subject.durableConsumerCreateSubject context stream durable)
consumerSubject context stream _ (NamedConsumer consumer) =
  Right (Subject.consumerCreateNamedSubject context stream consumer)
consumerSubject context stream _ (DurableConsumer durable) =
  Right (Subject.consumerCreateNamedSubject context stream durable)
consumerSubject _ _ _ EphemeralConsumer =
  Left (JetStreamDecodeError "ephemeral consumers can only be created")

createConsumerRequest :: StreamName -> Maybe StreamName -> ConsumerConfigRequest -> Value
createConsumerRequest stream action config =
  object . catMaybes $
    [ Just ("stream_name" .= byteStringToJSON stream)
    , Just ("config" .= toJSON config)
    , fmap (("action" .=) . byteStringToJSON) action
    ]
