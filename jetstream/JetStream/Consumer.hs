{-# LANGUAGE OverloadedStrings #-}

module JetStream.Consumer
  ( consumerAPI
  , module JetStream.Consumer.API
  ) where

import           Data.Aeson                 (Value, object, toJSON, (.=))
import           Data.Maybe                 (catMaybes)
import           JetStream.Consumer.API
import           JetStream.Consumer.Types
    ( ConsumerAPI (..)
    , ConsumerConfigRequest
    , applyConsumerKind
    , applyConsumerTarget
    , consumerActionValue
    , consumerConfigRequest
    , consumerListRequest
    , consumerNamesRequest
    , consumerPauseRequest
    , consumerResetRequest
    , validateConsumerConfigRequest
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
    { putConsumer = \stream action target kind options requestOptions ->
        case consumerRequest context stream action target kind options of
          Left err ->
            pure (Left err)
          Right (subject, actionValue, config) ->
            Request.requestJSON context
              subject
              (Just (createConsumerRequest stream actionValue config))
              requestOptions
    , consumerInfo = \stream consumer requestOptions ->
        Request.requestJSON context
          (Subject.consumerInfoSubject context stream consumer)
          Nothing
          requestOptions
    , pauseConsumer = \stream consumer pauseUntil requestOptions ->
        Request.requestJSON context
          (Subject.consumerPauseSubject context stream consumer)
          (Just (toJSON (consumerPauseRequest (Just pauseUntil))))
          requestOptions
    , resumeConsumer = \stream consumer requestOptions ->
        Request.requestJSON context
          (Subject.consumerPauseSubject context stream consumer)
          (Just (toJSON (consumerPauseRequest Nothing)))
          requestOptions
    , resetConsumer = \stream consumer options requestOptions ->
        Request.requestJSON context
          (Subject.consumerResetSubject context stream consumer)
          (Just (toJSON (consumerResetRequest options)))
          requestOptions
    , deleteConsumer = \stream consumer requestOptions ->
        Request.requestJSON context
          (Subject.consumerDeleteSubject context stream consumer)
          Nothing
          requestOptions
    , listConsumers = \stream options requestOptions ->
        Request.requestJSON context
          (Subject.consumerListSubject context stream)
          (Just (toJSON (consumerListRequest options)))
          requestOptions
    , consumerNames = \stream options requestOptions ->
        Request.requestJSON context
          (Subject.consumerNamesSubject context stream)
          (Just (toJSON (consumerNamesRequest options)))
          requestOptions
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
    Right subject -> do
      let config =
            applyConsumerTarget target . applyConsumerKind kind $
              consumerConfigRequest options
      validateConsumerConfigRequest config
      pure
        ( subject
        , actionValue
        , config
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
