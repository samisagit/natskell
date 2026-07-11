{-# LANGUAGE OverloadedStrings #-}

module JetStream.Protocol.Subject
  ( apiPrefix
  , apiSubject
  , accountInfoSubject
  , streamCreateSubject
  , streamUpdateSubject
  , streamInfoSubject
  , streamDeleteSubject
  , streamPurgeSubject
  , streamListSubject
  , streamNamesSubject
  , consumerCreateSubject
  , durableConsumerCreateSubject
  , consumerInfoSubject
  , consumerDeleteSubject
  , consumerListSubject
  , consumerNamesSubject
  , consumerNextSubject
  ) where

import qualified Data.ByteString   as BS
import           JetStream.Options (JetStreamContext (..))
import           JetStream.Types   (ConsumerName, StreamName, Subject)

apiPrefix :: JetStreamContext -> Subject
apiPrefix ctx =
  case contextDomain ctx of
    Nothing     -> "$JS.API"
    Just domain -> BS.concat ["$JS.", domain, ".API"]

apiSubject :: JetStreamContext -> [BS.ByteString] -> Subject
apiSubject ctx segments =
  BS.intercalate "." (apiPrefix ctx : segments)

accountInfoSubject :: JetStreamContext -> Subject
accountInfoSubject ctx =
  apiSubject ctx ["INFO"]

streamCreateSubject :: JetStreamContext -> StreamName -> Subject
streamCreateSubject ctx stream =
  apiSubject ctx ["STREAM", "CREATE", stream]

streamUpdateSubject :: JetStreamContext -> StreamName -> Subject
streamUpdateSubject ctx stream =
  apiSubject ctx ["STREAM", "UPDATE", stream]

streamInfoSubject :: JetStreamContext -> StreamName -> Subject
streamInfoSubject ctx stream =
  apiSubject ctx ["STREAM", "INFO", stream]

streamDeleteSubject :: JetStreamContext -> StreamName -> Subject
streamDeleteSubject ctx stream =
  apiSubject ctx ["STREAM", "DELETE", stream]

streamPurgeSubject :: JetStreamContext -> StreamName -> Subject
streamPurgeSubject ctx stream =
  apiSubject ctx ["STREAM", "PURGE", stream]

streamListSubject :: JetStreamContext -> Subject
streamListSubject ctx =
  apiSubject ctx ["STREAM", "LIST"]

streamNamesSubject :: JetStreamContext -> Subject
streamNamesSubject ctx =
  apiSubject ctx ["STREAM", "NAMES"]

consumerCreateSubject :: JetStreamContext -> StreamName -> Subject
consumerCreateSubject ctx stream =
  apiSubject ctx ["CONSUMER", "CREATE", stream]

durableConsumerCreateSubject :: JetStreamContext -> StreamName -> ConsumerName -> Subject
durableConsumerCreateSubject ctx stream consumer =
  apiSubject ctx ["CONSUMER", "DURABLE", "CREATE", stream, consumer]

consumerInfoSubject :: JetStreamContext -> StreamName -> ConsumerName -> Subject
consumerInfoSubject ctx stream consumer =
  apiSubject ctx ["CONSUMER", "INFO", stream, consumer]

consumerDeleteSubject :: JetStreamContext -> StreamName -> ConsumerName -> Subject
consumerDeleteSubject ctx stream consumer =
  apiSubject ctx ["CONSUMER", "DELETE", stream, consumer]

consumerListSubject :: JetStreamContext -> StreamName -> Subject
consumerListSubject ctx stream =
  apiSubject ctx ["CONSUMER", "LIST", stream]

consumerNamesSubject :: JetStreamContext -> StreamName -> Subject
consumerNamesSubject ctx stream =
  apiSubject ctx ["CONSUMER", "NAMES", stream]

consumerNextSubject :: JetStreamContext -> StreamName -> ConsumerName -> Subject
consumerNextSubject ctx stream consumer =
  apiSubject ctx ["CONSUMER", "MSG", "NEXT", stream, consumer]

