module Publish
  ( defaultPublishConfig
  ) where

import           Publish.Config (PublishConfig)

defaultPublishConfig :: PublishConfig
defaultPublishConfig = (Nothing, Nothing, Nothing)
