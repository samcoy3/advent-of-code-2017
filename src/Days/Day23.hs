module Days.Day23 (runDay) where

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
inputParser = undefined

------------ TYPES ------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = undefined

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
