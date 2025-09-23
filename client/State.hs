module State where

data ConnState = Connecting
               | Connected
               | Draining Severity Severity -- stream broadcast
               | Disconnected
  deriving (Eq, Show)

data Direction = Stream | Broadcast
  deriving (Eq, Show)

data Severity = Hard | Soft | None
  deriving (Eq, Ord, Show)

data ConnEvent = ConnectionEstablished
               | DrainingStarted (Direction, Severity)
               | DisconnectedByPeer
  deriving (Eq, Show)

transition :: ConnState -> ConnEvent -> Either String ConnState
transition Connecting ConnectionEstablished = Right Connected
transition Connected (DrainingStarted (Stream, severity)) = Right $ Draining severity None
transition Connected (DrainingStarted (Broadcast, severity)) = Right $ Draining None severity
transition (Draining s s') (DrainingStarted (Stream, severity)) = Right $ Draining (max s severity) s'
transition (Draining s s') (DrainingStarted (Broadcast, severity)) = Right $ Draining s (max s' severity)
transition Connected DisconnectedByPeer = Right Disconnected
transition s         e                      = Left $ "Invalid transition: " ++ show e ++ "applied to " ++ show s

