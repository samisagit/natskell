{-# LANGUAGE OverloadedStrings #-}

module Err where

import           Parser

data Err = Err deriving (Show, Eq)

parser :: Parser Err
parser = do
  string "-ERR\r\n"
  return Err
