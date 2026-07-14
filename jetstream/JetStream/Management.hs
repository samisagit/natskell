-- | Account-level JetStream management implementation.
module JetStream.Management (managementAPI) where

import           JetStream.Management.API   (ManagementAPI (..))
import           JetStream.Options          (JetStreamContext)
import qualified JetStream.Protocol.Request as Request
import qualified JetStream.Protocol.Subject as Subject

managementAPI :: JetStreamContext -> ManagementAPI
managementAPI context =
  ManagementAPI
    { accountInfo =
        Request.requestJSON context (Subject.accountInfoSubject context) Nothing
    }
