module Days.Day19 (runDay) where

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

import qualified Util.Parsers as P
import Control.Monad.State
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = P.coordinateParser mapper 0
  where
    mapper '-' = Just (Line Horizontal)
    mapper '|' = Just (Line Vertical)
    mapper '+' = Just (Line Turning)
    mapper c =
      if c `elem` ['A' .. 'Z']
        then Just (Letter c)
        else Nothing

------------ TYPES ------------
data LineType
  = Horizontal
  | Vertical
  | Turning
  deriving (Show, Eq)

data LineObject
  = Line LineType
  | Letter Char
  deriving (Show)

data Direction = N | S | E | W
  deriving (Show, Eq)

data Journey = Journey
  { direction :: Direction,
    pos :: (Int, Int),
    letters :: [Char],
    stepCount :: Int
  }

type JourneyM = State Journey

type Input = Map (Int, Int) LineObject

type OutputA = String

type OutputB = Int

------------ PART A ------------
stepInDirection :: (Int, Int) -> Direction -> (Int, Int)
stepInDirection (x, y) d = case d of
  N -> (x, y - 1)
  E -> (x + 1, y)
  S -> (x, y + 1)
  W -> (x - 1, y)

adjacentDirections :: Direction -> (Direction, Direction)
adjacentDirections N = (E, W)
adjacentDirections S = (E, W)
adjacentDirections E = (N, S)
adjacentDirections W = (N, S)

performJourney :: Input -> JourneyM ()
performJourney m = do
  nextPos <- stepInDirection <$> (gets pos) <*> (gets direction)
  modify
    ( \j ->
        j
          { pos = nextPos,
            stepCount = (stepCount j) + 1
          }
    )
  let nextItem = m Map.!? nextPos
  case nextItem of
    Nothing -> return ()
    Just (Letter c) -> modify (\j -> j {letters = (letters j) ++ [c]}) >> performJourney m
    Just (Line l) -> do
      dir <- gets direction
      if l /= Turning
        then performJourney m
        else
          let (d1, d2) = adjacentDirections dir
              o1 = m Map.!? (stepInDirection nextPos d1)
           in modify (\j -> j {direction = if isNothing o1 then d2 else d1}) >> performJourney m

startingState :: Input -> Journey
startingState m =
  Journey
    { direction = S,
      letters = [],
      pos = (\(a, b) -> (b, a)) . minimum . fmap (\(a, b) -> (b, a)) . Map.keys $ m,
      stepCount = 0
    }

partA :: Input -> OutputA
partA m = letters . flip execState (startingState m) $ performJourney m

------------ PART B ------------
partB :: Input -> OutputB
partB m = stepCount . flip execState (startingState m) $ performJourney m
