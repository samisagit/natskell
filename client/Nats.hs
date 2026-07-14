-- | The canonical entry point for the core NATS client API.
--
-- More specialised code can import "Nats.Client", "Nats.Message", or
-- "Nats.Subscription" directly. JetStream is available from
-- "Nats.JetStream".
module Nats (module Nats.Client) where

import           Nats.Client
