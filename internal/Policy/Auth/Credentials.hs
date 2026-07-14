{-# LANGUAGE OverloadedStrings #-}

module Auth.Credentials
  ( JwtBundle (..)
  , parseJwtBundle
  ) where

import qualified Data.ByteString as BS
import           Data.Word       (Word8)

data JwtBundle = JwtBundle
                   { jwtToken :: BS.ByteString
                   , jwtSeed  :: BS.ByteString
                   }
  deriving (Eq)

instance Show JwtBundle where
  show _ = "JwtBundle {jwtToken = <redacted>, jwtSeed = <redacted>}"

parseJwtBundle :: BS.ByteString -> Maybe JwtBundle
parseJwtBundle input = do
  jwt <- extractBlock jwtStart jwtEnd input
  seed <- extractBlock seedStart seedEnd input
  pure (JwtBundle jwt seed)

extractBlock :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
extractBlock startMarker endMarker input = do
  let (_, rest) = BS.breakSubstring startMarker input
  if BS.null rest
    then Nothing
    else do
      let afterStart = BS.drop (BS.length startMarker) rest
          (block, end) = BS.breakSubstring endMarker afterStart
          trimmed = trimAscii block
      if BS.null end || BS.null trimmed
        then Nothing
        else Just trimmed

jwtStart, jwtEnd, seedStart, seedEnd :: BS.ByteString
jwtStart = "-----BEGIN NATS USER JWT-----"
jwtEnd = "------END NATS USER JWT------"
seedStart = "-----BEGIN USER NKEY SEED-----"
seedEnd = "------END USER NKEY SEED------"

trimAscii :: BS.ByteString -> BS.ByteString
trimAscii =
  dropWhileEndAscii isSpaceAscii . BS.dropWhile isSpaceAscii

dropWhileEndAscii :: (Word8 -> Bool) -> BS.ByteString -> BS.ByteString
dropWhileEndAscii predicate =
  BS.reverse . BS.dropWhile predicate . BS.reverse

isSpaceAscii :: Word8 -> Bool
isSpaceAscii w =
  w == 9 || w == 10 || w == 13 || w == 32
