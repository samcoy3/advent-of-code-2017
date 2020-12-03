module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
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
  (,)
    <$> (string "Generator A starts with " >> decimal)
    <*> (string "\nGenerator B starts with " >> decimal)

------------ TYPES ------------
type Input = (Integer, Integer)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (a, b) =
  let as =
        unfoldr
          ( \a' ->
              let nextA = (16_807 * a') `mod` 2_147_483_647
               in Just (nextA `mod` 65_536, nextA)
          )
          a
      bs =
        unfoldr
          ( \b' ->
              let nextB = (48_271 * b') `mod` 2_147_483_647
               in Just (nextB `mod` 65_536, nextB)
          )
          b
   in length
        . filter id
        . L.take 40_000_000
        $ zipWith (==) as bs

------------ PART B ------------
partB :: Input -> OutputB
partB (a, b) =
  let as =
        unfoldr
          ( \a' ->
              let nextA = (16_807 * a') `mod` 2_147_483_647
               in Just (nextA `mod` 65_536, nextA)
          )
          a
      bs =
        unfoldr
          ( \b' ->
              let nextB = (48_271 * b') `mod` 2_147_483_647
               in Just (nextB `mod` 65_536, nextB)
          )
          b
   in length
        . filter id
        . L.take 5_000_000
        $ zipWith
          (==)
          (filter ((== 0) . (`mod` 4)) as)
          (filter ((== 0) . (`mod` 8)) bs)
