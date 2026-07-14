module Router.Nats
  ( RouteDirective (..)
  , routeMessage
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Lib.Logger             (LogLevel (..), MonadLogger (..))
import           Parser.API
    ( ParsedMessage (ParsedErr, ParsedInfo, ParsedMessageTooLarge, ParsedMsg, ParsedOk, ParsedPing, ParsedPong)
    )
import           Queue.API              (QueueItem (QueueItem))
import           State.Store
    ( ClientState
    , enqueue
    , runClient
    , runNextPingAction
    , setServerInfo
    , updateLogContextFromInfo
    )
import           State.Types
    ( ClientExitReason (ExitInboundMessageTooLarge, ExitServerError)
    , serverErrorFromProtocol
    )
import           Subscription.Store
    ( DispatchResult (DispatchDropped, DispatchMissing, DispatchQueued)
    , SubscriptionStore
    , dispatchMessage
    )
import qualified Types.Err              as Err
import qualified Types.Msg              as Msg
import           Types.Pong             (Pong (..))

data RouteDirective = RouteContinue
                    | RouteExit ClientExitReason
  deriving (Eq, Show)

routeMessage :: ClientState -> SubscriptionStore -> ParsedMessage -> IO RouteDirective
routeMessage state store parsed =
  runClient state $
    case parsed of
      ParsedMsg msg -> do
        logMessage Debug ("routing MSG: " ++ show msg)
        result <- liftIO $ dispatchMessage store msg
        case result of
          DispatchQueued ->
            pure RouteContinue
          DispatchDropped reportSlowConsumer -> do
            if reportSlowConsumer
              then
                logMessage Error "slow consumer: global pending delivery limit reached"
              else
                logMessage Debug "dropping delivery while client remains a slow consumer"
            pure RouteContinue
          DispatchMissing -> do
            logMessage Error ("callback missing for SID: " ++ show (Msg.sid msg))
            pure RouteContinue
      ParsedMessageTooLarge actual maximumSize -> do
        logMessage Error
          ( "inbound message size "
              ++ show actual
              ++ " exceeds client limit "
              ++ show maximumSize
          )
        pure (RouteExit (ExitInboundMessageTooLarge actual maximumSize))
      ParsedInfo info -> do
        logMessage Debug ("routing INFO: " ++ show info)
        liftIO $ setServerInfo state info
        liftIO $ updateLogContextFromInfo state info
        pure RouteContinue
      ParsedPing _ -> do
        logMessage Debug "routing PING"
        liftIO $ enqueue state (QueueItem Pong)
        pure RouteContinue
      ParsedPong _ -> do
        logMessage Debug "routing PONG"
        liftIO $ runNextPingAction state
        pure RouteContinue
      ParsedOk okMsg -> do
        logMessage Debug ("routing OK: " ++ show okMsg)
        pure RouteContinue
      ParsedErr err -> do
        logMessage Debug ("routing ERR: " ++ show err)
        if Err.isFatal err
          then do
            logMessage Error ("fatal server error: " ++ show err)
            pure (RouteExit (ExitServerError (serverErrorFromProtocol err)))
          else do
            logMessage Warn ("server error: " ++ show err)
            pure RouteContinue
