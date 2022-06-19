{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Control.Concurrent
import           Control.Exception
import qualified Data.ByteString    as BS
import qualified Info               as I
import qualified Network.Simple.TCP as TCP
import           Parser

connect :: String -> Int -> Int -> IO ()
connect host port retryCount = do
  (sock, _) <- TCP.connectSock host $ show port
  result <- try(
    TCP.recv sock 1000
    ) :: IO (Either SomeException (Maybe BS.ByteString))
  case result of
    Left e -> do
      if retryCount < 1
        then error $ "retry count exceeded " ++ show e
      else do
        print e
        threadDelay 1000000
        connect host port $ retryCount - 1
    Right info -> do
      case info of
        Just a -> do
          let result = fmap fst $ runParser I.parser a
          case result of
            Just a  -> return ()
            Nothing -> error $ "response incorrect: " ++ show a
        Nothing -> do
          if retryCount < 1
            then error $ "retry count exceeded, no response"
          else do
            threadDelay 1000000
            connect host port $ retryCount - 1
