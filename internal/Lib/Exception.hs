{-# LANGUAGE TypeApplications #-}

module Lib.Exception
  ( trySync
  ) where

import           Control.Exception

-- | Convert synchronous failures to values without swallowing cancellation.
trySync :: IO a -> IO (Either SomeException a)
trySync action = do
  result <- try @SomeException action
  case result of
    Left err ->
      case fromException err :: Maybe SomeAsyncException of
        Just _  -> throwIO err
        Nothing -> pure (Left err)
    Right value -> pure (Right value)
