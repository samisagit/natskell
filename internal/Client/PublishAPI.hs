module Client.PublishAPI
  ( Payload
  , Headers
  , PublishConfig
  , defaultPublishConfig
  ) where

import qualified Types.Msg as M
import           Types.Msg (Headers, Payload)

type PublishConfig = (Maybe Payload, Maybe (Maybe M.Msg -> IO ()), Maybe Headers)

defaultPublishConfig :: PublishConfig
defaultPublishConfig = (Nothing, Nothing, Nothing)
