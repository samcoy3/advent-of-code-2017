module Days.Day11 (runDay) where

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

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = hexPoint `sepBy` char ','
  where
    hexPoint =
      HexPoint
        <$> choice
          [ asciiCI "ne" >> return (1, 0),
            asciiCI "sw" >> return (-1, 0),
            asciiCI "se" >> return (1, -1),
            asciiCI "nw" >> return (-1, 1),
            char 'n' >> return (0, 1),
            char 's' >> return (0, -1)
          ]

------------ TYPES ------------
type Input = [HexPoint]

type OutputA = Int

type OutputB = Int

-- A hex-based co-ordinate system.
-- The "x" axis runs sw-ne, and the "y" axis runs s-n
newtype HexPoint = HexPoint {getPoint :: (Int, Int)}
  deriving (Eq, Ord, Show)

-- Hex-vector addition
(<+>) :: HexPoint -> HexPoint -> HexPoint
(<+>) (HexPoint (x1, y1)) (HexPoint (x2, y2)) = HexPoint (x1 + x2, y1 + y2)

------------ PART A ------------
-- Using a little known property of this hex coordinate system to extract the distance
distance :: HexPoint -> HexPoint -> Int
distance (HexPoint (x1, y1)) (HexPoint (x2, y2)) =
  let x = x2 - x1
      y = y2 - y1
      z = - (x + y)
   in (abs (x) + abs (y) + abs (z)) `div` 2

partA :: Input -> OutputA
partA = distance (HexPoint (0, 0)) . foldr (<+>) (HexPoint (0, 0))

------------ PART B ------------
partB :: Input -> OutputB
partB = maximum . fmap (distance (HexPoint (0, 0))) . scanl (<+>) (HexPoint (0, 0))
