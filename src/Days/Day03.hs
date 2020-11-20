module Days.Day03 (runDay) where

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
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal

------------ TYPES ------------
type Input = Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Finds the location of a number on the (part a) grid.
findLocation :: Int -> (Int, Int)
findLocation n =
  -- We find the nearest odd square below the target, and then go round the four sides until we find it.
  let nearestOddSquareRoot = (\x -> if x `mod` 2 == 0 then x - 1 else x) . floor . sqrt . fromIntegral $ n
      nearestOddSquare = floor . (** 2) . fromIntegral $ nearestOddSquareRoot
      sideLength = nearestOddSquareRoot + 1
      (dx, dy) =
        if
            | n == nearestOddSquare ->
              (0, 0)
            | n <= nearestOddSquare + sideLength ->
              (1, -1 * (n - nearestOddSquare - 1))
            | n <= nearestOddSquare + (2 * sideLength) ->
              (-1 * (n - (nearestOddSquare + sideLength) - 1), - sideLength + 1)
            | n <= nearestOddSquare + (3 * sideLength) ->
              (- (sideLength - 1), 1 + (n - (nearestOddSquare + (3 * sideLength))))
            | n <= nearestOddSquare + (4 * sideLength) ->
              (1 + n - (nearestOddSquare + (4 * sideLength)), 1)
            | otherwise ->
              error "Impossible"
   in (dx + (nearestOddSquareRoot `div` 2), dy + (nearestOddSquareRoot `div` 2))

-- Calculates the manhattan distance between two points
manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

partA :: Input -> OutputA
partA = manhattanDistance (0, 0) . findLocation

------------ PART B ------------
-- Gets a list of adjacent points (including diagonals) to a given point
adjacentPoints :: (Int, Int) -> [(Int, Int)]
adjacentPoints (x, y) = [(x', y') | x' <- [x -1 .. x + 1], y' <- [y -1 .. y + 1], x' /= x || y' /= y]

-- In this one we recursively draw the grid, until we draw a number greater than the input
partB :: Input -> OutputB
partB input = drawGrid 2 (Map.insert (0, 0) 1 Map.empty)
  where
    adjacentSum n m = sum $ fmap (flip (Map.findWithDefault 0) m) (adjacentPoints $ findLocation n)
    drawGrid n m =
      if
          | adjacentSum n m >= input -> adjacentSum n m
          | otherwise -> drawGrid (n + 1) (Map.insert (findLocation n) (adjacentSum n m) m)
