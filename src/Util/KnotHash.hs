module Util.KnotHash (knotHash, binarifyHash) where

import Data.Bits
import Data.List as L
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Text.Printf

-- Our initial list
standardList :: Vector Int
standardList = Vec.fromList [0 .. 255]

-- Reverses part of a vector, as per the question.
-- Supports index "wrapping"
reversePortion :: Int -> Int -> Vector a -> Vector a
reversePortion currentPosition len vec =
  let affectedIndices = fmap ((`mod` (Vec.length vec)) . (+ currentPosition)) [0 .. (len -1)]
      affectedValues = fmap (vec Vec.!) affectedIndices
   in (Vec.//) vec . zip affectedIndices $ reverse affectedValues

-- Performs 64 rounds of the hash
sixtyFourRounds :: [Int] -> Vector a -> Vector a
sixtyFourRounds ls vec = sixtyFourRounds' vec 0 0 1 ls
  where
    sixtyFourRounds' vec _ _ 64 [] = vec
    sixtyFourRounds' vec c s r [] = sixtyFourRounds' vec c s (r + 1) ls
    sixtyFourRounds' vec c s r (l : ls) =
      sixtyFourRounds'
        (reversePortion c l vec)
        ((c + l + s) `mod` Vec.length vec)
        (s + 1)
        r
        ls

-- Densifies the hash by xor'ing 16-element chunks of the hash
densifyHash :: Vector Int -> [Int]
densifyHash = densifyHash' . Vec.toList
  where
    densifyHash' [] = []
    densifyHash' l = (foldr1 xor . L.take 16 $ l) : (densifyHash' . drop 16 $ l)

-- Converts the xor'ed parts of the hash to a length-2 hex string
hexify :: Int -> String
hexify = printf "%02x"

knotHash :: [Int] -> String
knotHash = concat . fmap hexify . densifyHash . (flip sixtyFourRounds) standardList . (++ [17, 31, 73, 47, 23])

binarifyHash :: String -> String
binarifyHash = concat . fmap binarify
  where
    binarify c = case c of
      '0' -> "0000"
      '1' -> "0001"
      '2' -> "0010"
      '3' -> "0011"
      '4' -> "0100"
      '5' -> "0101"
      '6' -> "0110"
      '7' -> "0111"
      '8' -> "1000"
      '9' -> "1001"
      'a' -> "1010"
      'b' -> "1011"
      'c' -> "1100"
      'd' -> "1101"
      'e' -> "1110"
      'f' -> "1111"
      _ -> error "Not a hex char!"
