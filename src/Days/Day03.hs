module Days.Day03 (runDay) where

import Data.Attoparsec.Text
import Data.List
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
  input <- readFile "input/Day03.txt" >>= (return . parseOnly inputParser . pack)
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
inputParser = decimal

------------ TYPES ------------
type Input = Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
findLocation :: Int -> (Int, Int)
findLocation n =
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

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

partA :: Input -> OutputA
partA = manhattanDistance (0, 0) . findLocation

------------ PART B ------------
adjacentPoints :: (Int, Int) -> [(Int, Int)]
adjacentPoints (x, y) = [(x', y') | x' <- [x -1 .. x + 1], y' <- [y -1 .. y + 1], x' /= x || y' /= y]

partB :: Input -> OutputB
partB input = drawGrid 2 (Map.insert (0, 0) 1 Map.empty)
  where
    adjacentSum n m = sum $ fmap (flip (Map.findWithDefault 0) m) (adjacentPoints $ findLocation n)
    drawGrid n m =
      if
          | adjacentSum n m >= input -> adjacentSum n m
          | otherwise -> drawGrid (n + 1) (Map.insert (findLocation n) (adjacentSum n m) m)
