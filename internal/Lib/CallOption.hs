module Lib.CallOption where

type CallOption a = (a -> a)

applyCallOptions :: [CallOption a] -> a -> a
applyCallOptions options value = foldr ($) value options
