module Days.Day01 (runDay) where

import Data.Attoparsec.Text
import Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Void

------------ DAY LOGIC ------------
runDay :: IO ()
runDay = do
  input <- readFile "input/Day01.txt" >>= (return . parseOnly inputParser . pack)
  processInput input
  where
    processInput (Left x) = error x
    processInput (Right i) = do
      putStrLn "Part A:"
      print $ partA i
      putStrLn "Part B:"
      print $ partB i

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
