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

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA input = length . filter (== '1') . (concatMap (binarifyHash . knotHash . fmap ord . ((input ++ "-") ++) . show)) $ [0 .. 127]

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
