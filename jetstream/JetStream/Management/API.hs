-- | Account-level JetStream management operations.
module JetStream.Management.API
  ( ManagementAPI (..)
  ) where

import           JetStream.Error (JetStreamError)
import           JetStream.Types (AccountInfo, JetStreamRequestOption)

-- | Account and tier management capabilities.
newtype ManagementAPI = ManagementAPI { accountInfo :: [JetStreamRequestOption] -> IO (Either JetStreamError AccountInfo) }
