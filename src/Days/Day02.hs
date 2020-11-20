module Days.Day02 (runDay) where

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

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  (decimal `sepBy1` (many1 $ char '\t'))
    `sepBy1` endOfLine

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
    -- Finds an evenly dividing pair and performs the division
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
