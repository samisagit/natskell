module Pipeline.Status where

data Status = Connecting
            | Connected
            | Disconnecting String
            | Disconnected String
            | Draining
  deriving (Eq, Show)

data Event = UnexpectedDisconnect String
           | ConnectedEvent
           | FatalErrorEvent String
           | UserCloseEvent
  deriving (Eq, Show)

