module Days.Day16 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Monad ((>=>))
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
inputParser = move `sepBy` char ','
  where
    move = choice [spin, exchange, partner]
    spin = char 's' >> decimal >>= return . Spin
    exchange = do
      char 'x'
      a <- decimal
      char '/'
      b <- decimal
      return (Exchange a b)
    partner = do
      char 'p'
      a <- anyChar
      char '/'
      b <- anyChar
      return (Partner a b)

------------ TYPES ------------
data DanceMove
  = Spin Int
  | Exchange Int Int
  | Partner Char Char
  deriving (Show)

type Input = [DanceMove]

type OutputA = String

type OutputB = String

------------ PART A ------------
perform :: DanceMove -> (Vector Char -> Vector Char)
perform (Spin n) = \v ->
  let (begin, end) = Vec.splitAt (16 - n) v
   in end Vec.++ begin
perform (Exchange a b) = \v ->
  v Vec.// [(a, v Vec.! b), (b, v Vec.! a)]
perform (Partner a b) = \v ->
  let aLoc = fromJust $ Vec.findIndex (== a) v
      bLoc = fromJust $ Vec.findIndex (== b) v
   in v Vec.// [(aLoc, b), (bLoc, a)]

partA :: Input -> OutputA
partA moves = Vec.toList . foldr1 (flip (.)) (fmap perform moves) . Vec.fromList $ ['a' .. 'p']

------------ PART B ------------
-- Given a map, create a new map by performing lookups from the previous map 10 times.
dectupleMap :: (Ord a) => Map a a -> Map a a
dectupleMap m =
  let tenMapLookups = foldr1 (.) (replicate 10 (m Map.!))
   in Map.fromList . fmap (\k -> (k, tenMapLookups k)) . Map.keys $ m

-- Creates an infinite list of a map, that map performed 10 times, that map performed 100 times, and so on
powerOfTenList :: (Ord a) => Map a a -> [Map a a]
powerOfTenList = unfoldr (\m -> let m' = dectupleMap m in Just (m', m'))

-- We need to separate dance moves that care about the integer position from those that care about the character
isPositional :: DanceMove -> Bool
isPositional (Partner _ _) = False
isPositional _ = True

-- Converts a series of dance moves to a map giving a character's position before and after the moves have been performed
oneDanceToLookup :: [DanceMove] -> Map Char Char
oneDanceToLookup moves = Map.fromList . zip ['a' .. 'p'] . Vec.toList . foldr1 (flip (.)) (fmap perform moves) . Vec.fromList $ ['a' .. 'p']

partB :: Input -> OutputB
partB input =
  let posSwaps = filter isPositional input
      charSwaps = filter (not . isPositional) input
      posPowerOfTenList = powerOfTenList $ oneDanceToLookup posSwaps
      charPowerOfTenList = powerOfTenList $ oneDanceToLookup charSwaps
   in fmap ((Map.!) (charPowerOfTenList !! 10) . (Map.!) (posPowerOfTenList !! 10)) $ ['a' .. 'p']
