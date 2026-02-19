module Nuid (Nuid, newNuid, newNuidIO, nextNuid) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Word             (Word64, Word8)
import           System.Random         (StdGen, newStdGen, randomR)

data Nuid = Nuid
              { nuidPrefix :: BS.ByteString
              , nuidSeq    :: Word64
              , nuidInc    :: Word64
              , nuidRng    :: StdGen
              }

newNuid :: StdGen -> Nuid
newNuid rng0 = Nuid prefix seqVal incVal rng3
  where
    (prefix, rng1) = randomPrefix rng0
    (seqVal, rng2) = randomSeq rng1
    (incVal, rng3) = randomInc rng2

newNuidIO :: IO Nuid
newNuidIO = newNuid <$> newStdGen

nextNuid :: Nuid -> (BS.ByteString, Nuid)
nextNuid nuid =
  let seqVal = nuidSeq nuid + nuidInc nuid
  in if seqVal >= maxSeq
      then resetAndNext nuid
      else let nuid' = nuid { nuidSeq = seqVal }
           in (renderNuid (nuidPrefix nuid) seqVal, nuid')

resetAndNext :: Nuid -> (BS.ByteString, Nuid)
resetAndNext nuid = (renderNuid prefix seqVal, Nuid prefix seqVal incVal rng3)
  where
    (prefix, rng1) = randomPrefix (nuidRng nuid)
    (seqVal, rng2) = randomSeq rng1
    (incVal, rng3) = randomInc rng2

renderNuid :: BS.ByteString -> Word64 -> BS.ByteString
renderNuid prefix seqVal = BS.append prefix (encodeSeq seqVal)

randomPrefix :: StdGen -> (BS.ByteString, StdGen)
randomPrefix rng0 = go prefixLen rng0 []
  where
    go 0 rng acc = (BS.pack (reverse acc), rng)
    go n rng acc =
      let (idx, rng') = randomR (0, base62Len - 1) rng
      in go (n - 1) rng' (base62Index idx : acc)

encodeSeq :: Word64 -> BS.ByteString
encodeSeq value = BS.pack (map base62Index (buildDigits seqLen value []))
  where
    buildDigits 0 _ acc = acc
    buildDigits n v acc =
      let (q, r) = v `quotRem` base
      in buildDigits (n - 1) q (fromIntegral r : acc)

base62Index :: Int -> Word8
base62Index = BS.index base62Alphabet

base62Alphabet :: BS.ByteString
base62Alphabet = BC.pack "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

base62Len :: Int
base62Len = BS.length base62Alphabet

prefixLen :: Int
prefixLen = 12

seqLen :: Int
seqLen = 10

base :: Word64
base = 62

maxSeq :: Word64
maxSeq = base ^ seqLen

minInc :: Word64
minInc = 33

maxInc :: Word64
maxInc = 333

randomSeq :: StdGen -> (Word64, StdGen)
randomSeq = randomR (0, maxSeq - 1)

randomInc :: StdGen -> (Word64, StdGen)
randomInc = randomR (minInc, maxInc - 1)
