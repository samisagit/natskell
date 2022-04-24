{-# LANGUAGE OverloadedStrings #-}

module Ok where

import           Parser

data Ok = Ok deriving (Show, Eq)

parser :: Parser Ok
parser = do
  string "+OK\r\n"
  return Ok
