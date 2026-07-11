module Publish.Config
  ( Payload
  , Headers
  , PublishConfig
  ) where

import qualified Types.Msg as M
import           Types.Msg (Headers, Payload, Subject)

type PublishConfig = (Maybe Payload, Maybe (Maybe M.Msg -> IO ()), Maybe Headers, Maybe Subject)
