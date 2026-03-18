module Client.PublishAPI
  ( PublishConfig
  , defaultPublishConfig
  ) where

import           Types     (Headers, Payload)
import qualified Types.Msg as M

type PublishConfig = (Maybe Payload, Maybe (Maybe M.Msg -> IO ()), Maybe Headers)

defaultPublishConfig :: PublishConfig
defaultPublishConfig = (Nothing, Nothing, Nothing)
