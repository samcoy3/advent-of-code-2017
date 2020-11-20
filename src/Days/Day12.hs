module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Data.Graph (Graph)
import qualified Data.Graph as Graph
import Data.Maybe
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = ((\(a, _, b) -> (a, b)) . Graph.graphFromEdges) <$> adjacencyLine `sepBy` endOfLine
  where
    adjacencyLine = do
      vertex <- decimal
      asciiCI " <-> "
      connections <- decimal `sepBy` asciiCI ", "
      return (vertex, vertex, connections)

------------ TYPES ------------
type Indexer = Int -> Maybe Int

type Input = (Graph, Indexer)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (g, i) = length . head $ Graph.dfs g [(fromJust $ i 0)]

------------ PART B ------------
partB :: Input -> OutputB
partB (g, i) = length $ Graph.dff g
