{-# LANGUAGE OverloadedStrings #-}

module Ping where

import           Parser

data Ping = Ping deriving (Show, Eq)

parser :: Parser Ping
parser = do
  string "PING\r\n"
  return Ping
