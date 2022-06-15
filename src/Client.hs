{-# LANGUAGE OverloadedStrings #-}

module Client where

import qualified Info               as I
import qualified Network.Simple.TCP as TCP
import           Parser

data Api = Api
  {
    host :: String,
    port :: String
  }

connect :: Api -> IO ()
connect a = do
  TCP.connect h p $ \(sock, _) -> do
    info <- TCP.recv sock 1000
    case info of
      Just a -> do
        let result = fmap fst $ runParser I.parser a
        case result of
          Just a  -> return ()
          Nothing -> error $ "response incorrect" ++ show a
      Nothing -> error "no response from server"
  where
    h = host a
    p = port a
