module Days.Day02 (runDay) where

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
  input <- readFile "input/Day02.txt" >>= (return . parseOnly inputParser . pack)
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
inputParser = (decimal `sepBy1` (many1 $ char '\t')) `sepBy1` endOfLine

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . (fmap rowChecksum)
  where
    rowChecksum row = (maximum row) - (minimum row)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . (fmap rowChecksum)
  where
    rowChecksum [] = error "No evenly-dividing pair!"
    rowChecksum (x : xs) = case findMatch x xs of
      Just n -> n
      Nothing -> rowChecksum xs
    findMatch x [] = Nothing
    findMatch x (y : ys) =
      if
          | x `mod` y == 0 -> Just $ x `div` y
          | y `mod` x == 0 -> Just $ y `div` x
          | otherwise -> findMatch x ys
