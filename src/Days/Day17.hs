module Days.Day17 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Data.List.PointedList as P
import qualified Data.List.PointedList.Circular as C

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal

------------ TYPES ------------
type Input = Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
insertValues :: Int -> Int -> Int
insertValues = insertValues' (P.singleton 0) 1
  where
    insertValues' plist value target step
      | value > target = P._focus . C.next $ plist
      | otherwise =
        let newPlist = P.insert value . foldr1 (.) (replicate step C.next) $ plist
         in insertValues' newPlist (value + 1) target step

partA :: Input -> OutputA
partA = insertValues 2017

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
