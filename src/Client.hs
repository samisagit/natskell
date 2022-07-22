{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Control.Concurrent
import           Control.Exception
import qualified Data.ByteString    as BS
import qualified Network.Simple.TCP as TCP
import           Parsers.Parsers

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
    Right bs -> do
      case bs of
        Just a -> do
          case genericParse a of
            Just result ->
              case result of
                ParsedInfo a -> return ()
                _            -> error $ "response incorrect: " ++ show a
            Nothing -> do
                error "parser failed to read a valid message"
        Nothing -> do
          if retryCount < 1
            then error "retry count exceeded, no message recieved"
          else do
            threadDelay 1000000
            connect host port $ retryCount - 1
