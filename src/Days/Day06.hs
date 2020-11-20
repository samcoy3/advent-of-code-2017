module Days.Day06 (runDay) where

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
inputParser =
  Vec.fromList
    <$> decimal `sepBy1` char '\t'

------------ TYPES ------------
type Input = Vector Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- This calculates the time until a function on a type "loops"
timeUntilCycle :: (Eq a) => (a -> a) -> a -> Int
timeUntilCycle f init = timeUntilCycle' [init] f init
  where
    timeUntilCycle' acc f current =
      if
          | f current `elem` acc -> length acc
          | otherwise -> timeUntilCycle' ((f current) : acc) f (f current)

-- This performs one cycle of the redistribution
performOneCycle :: Vector Int -> Vector Int
performOneCycle banks =
  let max = Vec.maxIndex banks
      valueInMax = banks Vec.! max
      redistributees = fmap ((`mod` (Vec.length banks)) . (+ max)) [1 .. valueInMax]
      incCount Nothing = Just 1
      incCount (Just a) = Just (a + 1)
      vecUpdates = Map.toList $ foldr (Map.alter incCount) Map.empty redistributees
      vecUpdatesAsVec = (Vec.replicate (Vec.length banks) 0) Vec.// vecUpdates
   in Vec.zipWith (+) vecUpdatesAsVec $ banks Vec.// [(max, 0)]

partA :: Input -> OutputA
partA = timeUntilCycle performOneCycle

------------ PART B ------------
-- Like above, this function calculates the time between the first repeat and the first "three-peat"
-- It calls the function above when it hits a repeat for the first time
timeUntilSecondCycle :: (Eq a) => (a -> a) -> a -> Int
timeUntilSecondCycle f init = timeUntilSecondCycle' [init] f init
  where
    timeUntilSecondCycle' acc f current =
      if
          | f current `elem` acc -> timeUntilCycle f (f current)
          | otherwise -> timeUntilSecondCycle' ((f current) : acc) f (f current)

partB :: Input -> OutputB
partB = timeUntilSecondCycle performOneCycle
