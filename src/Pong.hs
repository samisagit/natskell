{-# LANGUAGE OverloadedStrings #-}

module Pong where

import           Parser

data Pong = Pong deriving (Show, Eq)

parser :: Parser Pong
parser = do
  string "PONG\r\n"
  return Pong
