module Publish
  ( defaultPublishConfig
  ) where

import           Publish.Config (PublishConfig (..))

defaultPublishConfig :: PublishConfig
defaultPublishConfig = PublishConfig Nothing Nothing
