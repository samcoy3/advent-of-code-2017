module Days.Day13 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Data.Function (on)

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = convertToVector <$> scanner `sepBy` endOfLine
  where
    scanner = do
      depth <- decimal
      asciiCI ": "
      range <- decimal
      return (depth, range)
    convertToVector l = Vec.replicate ((maximum $ fmap fst l) + 1) 0 Vec.// l

------------ TYPES ------------
type Input = Vector Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = foldr1 (+) . Vec.imap severity
  where
    severity i val =
      if i `mod` (2 * (val - 1)) == 0
        then val * i
        else 0

------------ PART B ------------
partB :: Input -> OutputB
partB scanners =
  head
    . foldr1
      (.)
      ( (fmap (\f -> filter f)) . Vec.toList $
          Vec.imap
            ( \i v d ->
                if (v == 0)
                  then True
                  else ((i + d) `mod` (2 * (v - 1)) /= 0)
            )
            scanners
      )
    $ [0 ..]
