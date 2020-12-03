module Days.Day22 (runDay) where

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

import Util.Parsers (coordinateParser)
import Control.Monad.State
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser (flip lookup [('.', Clean), ('#', Infected)]) 0

------------ TYPES ------------
data Direction = N | E | S | W deriving (Eq, Show)

data Status = Infected | Clean deriving (Eq, Show)

data Carrier = Carrier
  { dir :: Direction,
    pos :: (Int, Int),
    environment :: Map (Int, Int) Status
  }
  deriving (Eq, Show)

type VirusM = State Carrier

type Input = Map (Int, Int) Status

type OutputA = Int

type OutputB = Void

------------ PART A ------------
move :: VirusM ()
move = do
  d <- gets dir
  modify
    ( \c@Carrier {..} ->
        let (x, y) = pos
         in c
              { pos =
                  ( case d of
                      N -> (x, y - 1)
                      E -> (x + 1, y)
                      S -> (x, y + 1)
                      W -> (x - 1, y)
                  )
              }
    )

turnLeft :: Direction -> Direction
turnLeft d = case d of
  N -> W
  W -> S
  S -> E
  E -> N

turnRight :: Direction -> Direction
turnRight d = case d of
  N -> E
  E -> S
  S -> W
  W -> N

burst :: VirusM Bool
burst = do
  infected <- gets (\c -> Map.findWithDefault Clean (pos c) (environment c) == Infected)
  if infected
    then modify (\c -> c {dir = turnRight (dir c)})
    else modify (\c -> c {dir = turnLeft (dir c)})
  if infected
    then modify (\c -> c {environment = Map.insert (pos c) Clean (environment c)})
    else modify (\c -> c {environment = Map.insert (pos c) Infected (environment c)})
  move
  return (not infected)

partA :: Input -> OutputA
partA e =
  let maxX = maximum . fmap fst . Map.keys $ e
      maxY = maximum . fmap snd . Map.keys $ e
   in length
        . filter id
        . flip evalState (Carrier {pos = (maxX `div` 2, maxY `div` 2), dir = N, environment = e})
        . sequence
        $ fmap (const burst) [1 .. 10_000]

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
