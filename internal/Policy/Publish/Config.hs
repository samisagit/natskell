module Publish.Config
  ( Headers
  , PublishConfig (..)
  ) where

import           Types.Msg (Headers, Subject)

data PublishConfig = PublishConfig
                       { publishHeaders :: Maybe Headers
                       , publishReplyTo :: Maybe Subject
                       }
