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

import Control.Monad.State
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
data SimplifiedSpinlock = SimplifiedSpinlock
  { pos :: Int,
    len :: Int,
    rightOfZero :: Int
  }
  deriving (Show)

type Day17 = State SimplifiedSpinlock

spin :: Int -> Int -> Day17 ()
spin steps n = do
  modify
    ( \s@SimplifiedSpinlock {..} ->
        s
          { pos = (pos + steps) `mod` len,
            len = len + 1
          }
    )
  newPos <- gets pos
  modify
    ( \s@SimplifiedSpinlock {..} ->
        s {pos = pos + 1}
    )
  when (newPos == 0) $
    modify
      ( \s ->
          s
            { rightOfZero = n
            }
      )

-- partB :: Input -> OutputB
partB steps =
  rightOfZero
    . flip execState (SimplifiedSpinlock {pos = 0, len = 1, rightOfZero = 0})
    . sequence_
    . fmap (spin steps)
    $ [1 .. 50_000_000]
