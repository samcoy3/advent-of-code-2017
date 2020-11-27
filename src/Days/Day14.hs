module Days.Day14 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Util.KnotHash
import Data.Char

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = init <$> many1 anyChar

------------ TYPES ------------
type Input = String

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = length . filter (== '1') . (concatMap (binarifyHash . knotHash . fmap ord . ((input ++ "-") ++) . show)) $ [0 .. 127]

------------ PART B ------------
type Coordinate = (Int, Int)

neighbours :: (Coordinate -> Bool) -> Coordinate -> Set Coordinate
neighbours p (x, y) =
  Set.fromList
    . filter p
    $ [ (x + 1, y),
        (x, y + 1),
        (x - 1, y),
        (x, y - 1)
      ]

connectedToExisting :: (Coordinate -> Bool) -> Set Coordinate -> Coordinate -> Maybe Coordinate
connectedToExisting p components coord = connectedToExisting' components (Set.empty) (Set.singleton coord)
  where
    connectedToExisting' components prevs currents =
      let nexts = Set.union currents $ Set.unions (Set.map (neighbours p) currents)
       in if
              | currents == nexts -> Just coord
              | not . Set.null $ Set.intersection nexts components -> Nothing
              | otherwise -> connectedToExisting' components currents nexts

nestedListsToMap :: [[a]] -> Map (Int, Int) a
nestedListsToMap = Map.fromList . attachCoords 0 0
  where
    attachCoords _ _ ([]) = []
    attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
    attachCoords x y ((l : ls) : lss) = ((x, y), l) : (attachCoords x (y + 1) (ls : lss))

partB :: Input -> OutputB
partB input =
  let hash = (fmap (binarifyHash . knotHash . fmap ord . ((input ++ "-") ++) . show)) $ [0 .. 127]
      hashMap = nestedListsToMap hash
      ones = Map.keys . Map.filter (== '1') $ hashMap
      findComponents comps [] = comps
      findComponents comps (x : xs) = case connectedToExisting (\k -> Map.findWithDefault '0' k hashMap == '1') comps x of
        Nothing -> findComponents comps xs
        Just y -> findComponents (Set.insert y comps) xs
   in Set.size $ findComponents Set.empty ones
