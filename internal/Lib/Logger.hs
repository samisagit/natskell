module Lib.Logger where

data Logger = Logger
                { logInfo  :: String -> IO ()
                , logError :: String -> IO ()
                , logDebug :: String -> IO ()
                , logFatal :: String -> IO ()
                }

