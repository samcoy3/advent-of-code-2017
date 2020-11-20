module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (digit >>= return . read . pure)

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Rotates a list by a given number
shift :: Int -> [Int] -> [Int]
shift n list = (drop n list) ++ (L.take n list)

-- Calculates the captchaScore of a pair of numbers
captchaScore :: Int -> Int -> Int
captchaScore a b = if a == b then a else 0

partA :: Input -> OutputA
partA input =
  foldr1 (+) $
    zipWith captchaScore input (shift 1 input)

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  foldr1 (+) $
    zipWith captchaScore input (shift (length input `div` 2) input)
