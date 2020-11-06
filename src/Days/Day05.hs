module Days.Day05 (runDay) where

import Data.Attoparsec.Text
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Void

------------ DAY LOGIC ------------
runDay :: IO ()
runDay = do
  input <- readFile "input/Day05.txt" >>= (return . parseOnly inputParser . pack)
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
inputParser = signed decimal `sepBy1` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
performJumps :: (Int -> Int) -> [Int] -> Int
performJumps updater = performJumps' 0 0 . Vec.fromList
  where
    performJumps' steps pointer jumps =
      if
          | jumps Vec.!? pointer == Nothing -> steps
          | otherwise ->
            performJumps'
              (steps + 1)
              (pointer + (jumps Vec.! pointer))
              (jumps Vec.// [(pointer, updater (jumps Vec.! pointer))])

partA :: Input -> OutputA
partA = performJumps (+ 1)

------------ PART B ------------

partB :: Input -> OutputB
partB = performJumps (\v -> if v >= 3 then v - 1 else v + 1)
