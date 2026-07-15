module Router.Nats
  ( RouteDirective (..)
  , routeMessage
  ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS
import           Lib.Logger             (LogLevel (..), MonadLogger (..))
import           Parser.API
    ( ParsedMessage (ParsedErr, ParsedInfo, ParsedMessageTooLarge, ParsedMsg, ParsedOk, ParsedPing, ParsedPong)
    )
import           Queue.API
    ( QueueItem (QueueConnectionScoped, QueueItem)
    )
import           State.Store
    ( ClientState
    , ServerErrorEnqueueResult (..)
    , enqueue
    , notifyServerError
    , resolveNextPing
    , runClient
    , setServerInfo
    , updateLogContextFromInfo
    )
import           State.Types
    ( ClientExitReason (ExitInboundMessageTooLarge, ExitServerError)
    , serverErrorFromProtocol
    , serverErrorKind
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
                    | RouteReconnect
                    | RouteExit ClientExitReason
  deriving (Eq, Show)

routeMessage :: ClientState -> SubscriptionStore -> ParsedMessage -> IO RouteDirective
routeMessage state store parsed =
  runClient state $
    case parsed of
      ParsedMsg msg -> do
        logMessage Debug
          ( "routing MSG: subject="
              ++ show (Msg.subject msg)
              ++ " sid="
              ++ show (Msg.sid msg)
              ++ " payload_bytes="
              ++ show (maybe 0 BS.length (Msg.payload msg))
              ++ " header_count="
              ++ show (maybe 0 length (Msg.headers msg))
          )
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
        logMessage Debug "routing INFO"
        liftIO $ setServerInfo state info
        liftIO $ updateLogContextFromInfo state info
        pure RouteContinue
      ParsedPing _ -> do
        logMessage Debug "routing PING"
        result <- liftIO $ enqueue state (QueueConnectionScoped (QueueItem Pong))
        pure $
          case result of
            Left _   -> RouteReconnect
            Right () -> RouteContinue
      ParsedPong _ -> do
        logMessage Debug "routing PONG"
        liftIO $ resolveNextPing state
        pure RouteContinue
      ParsedOk okMsg -> do
        logMessage Debug ("routing OK: " ++ show okMsg)
        pure RouteContinue
      ParsedErr err -> do
        let serverError = serverErrorFromProtocol err
            kind = serverErrorKind serverError
        logMessage Debug ("routing ERR: kind=" ++ show kind)
        enqueueResult <- liftIO $ notifyServerError state serverError
        let accepted = enqueueResult == ServerErrorQueued
        when (enqueueResult == ServerErrorDroppedReport) $
          logMessage Warn "server error handler queue full; dropping event"
        if Err.isFatal err
          then do
            logMessage Error ("fatal server error: kind=" ++ show kind)
            pure (RouteExit (ExitServerError serverError))
          else if accepted
          then do
            logMessage Warn ("server error: kind=" ++ show kind)
            pure RouteContinue
          else pure RouteContinue
