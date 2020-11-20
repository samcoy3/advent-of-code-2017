module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Text (pack)
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = signed decimal `sepBy1` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Performs the instruction jumps according to the specification
performJumps :: (Int -> Int) -> [Int] -> Int
performJumps updater = performJumps' 0 0 . Vec.fromList
  where
    performJumps' steps pointer jumps =
      if
          -- If we jump outside the vector then we're finished
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
