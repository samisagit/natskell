module Client.CallbacksAPI
  ( CallbacksAPI (..)
  ) where

import           Client.RuntimeAPI (ClientState)

data CallbacksAPI = CallbacksAPI
                      { callbacksEnqueue      :: ClientState -> IO () -> IO ()
                      , callbacksStartWorkers :: ClientState -> IO ()
                      , callbacksAwaitDrain   :: ClientState -> IO ()
                      }
