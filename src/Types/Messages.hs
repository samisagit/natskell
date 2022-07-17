{-# LANGUAGE OverloadedStrings #-}
module Types.Messages where

import           Control.Applicative
import           Data.ByteString
import qualified Data.ByteString     as BS
import           Parser

data Generic = ParsedPing Ping | ParsedPong Pong

data Ping = Ping deriving (Show, Eq)

iparser :: Parser Generic
iparser = do
  string "PING\r\n"
  return (ParsedPing Ping)

itransform :: Ping -> ByteString
itransform _ = "PING"

data Pong = Pong deriving (Show, Eq)

oparser :: Parser Generic
oparser = do
  string "PONG\r\n"
  return (ParsedPong Pong)

otransform :: Pong -> ByteString
otransform _ = "PONG"

genericParse :: BS.ByteString -> IO ()
genericParse a = case (runParser (oparser <|> iparser) a) of
  Just (p, _) -> case p of
    ParsedPing a -> print "parsed a ping"
    ParsedPong a -> print "parsed a pong"
  Nothing     -> return ()
