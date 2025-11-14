module Lib.CallOption where

type CallOption a = (a -> a)

applyCallOptions :: [CallOption a] -> a -> a
applyCallOptions opts value = foldr ($) value opts
