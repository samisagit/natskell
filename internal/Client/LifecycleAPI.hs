module Client.LifecycleAPI
  ( ClientExitReason (..)
  , LifecycleState (..)
  , LifecycleAPI (..)
  ) where

import           Control.Concurrent.STM (STM)
import qualified Types.Err              as Err
import qualified Types.Info             as I

data ClientExitReason = ExitClosedByUser
                      | ExitRetriesExhausted (Maybe String)
                      | ExitServerError Err.Err
                      | ExitResetRequested
  deriving (Eq, Show)

data LifecycleState = Running
                    | Closing ClientExitReason
                    | Closed ClientExitReason

data LifecycleAPI s = LifecycleAPI
                        { lifecycleUpdateLogContextFromInfo :: s -> I.Info -> IO ()
                        , lifecycleSetServerInfo :: s -> I.Info -> STM ()
                        , lifecycleSetClosing :: s -> ClientExitReason -> STM ()
                        , lifecycleMarkClosed :: s -> ClientExitReason -> STM (Maybe ClientExitReason)
                        , lifecycleWaitForClosed :: s -> STM ()
                        , lifecycleWaitForServerInfo :: s -> STM ()
                        , lifecycleWaitForClosing :: s -> STM ()
                        }
