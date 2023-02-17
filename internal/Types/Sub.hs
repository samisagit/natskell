{-# LANGUAGE OverloadedStrings #-}

module Types.Sub where

import qualified Data.ByteString       as BS
import           Validators.Validators

data Sub = Sub
  { subject    :: BS.ByteString,
    queueGroup :: Maybe BS.ByteString,
    sid        :: BS.ByteString
  }
  deriving (Eq, Show)

instance Validator Sub where
  validate s
    | subject s == "" = Just "explicit empty subject"
    | queueGroup s == Just "" = Just "explicit empty queue group"
    | sid s == "" = Just "explicit empty sid"
    | otherwise = Nothing
