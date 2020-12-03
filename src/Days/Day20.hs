module Days.Day20 (runDay) where

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

import Data.Function (on)
import Control.Applicative.Combinators as C
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = particle `C.sepBy` endOfLine
  where
    particle = do
      string "p="
      position <- vector3
      string ", v="
      velocity <- vector3
      string ", a="
      acceleration <- vector3
      return Particle {..}
    vector3 = C.between (char '<') (char '>') $ do
      x <- signed decimal
      char ','
      y <- signed decimal
      char ','
      z <- signed decimal
      return Vec3 {..}

------------ TYPES ------------
data Vec3 = Vec3 {x :: Int, y :: Int, z :: Int} deriving (Eq, Show)

data Particle = Particle {position :: Vec3, velocity :: Vec3, acceleration :: Vec3} deriving (Eq, Show)

type Input = [Particle]

type OutputA = Int

type OutputB = Void

------------ PART A ------------
manhattan :: Vec3 -> Int
manhattan Vec3 {..} = (abs x) + (abs y) + (abs z)

partA :: Input -> OutputA
partA ps =
  let ips = zip [0 ..] ps
      highestAccel = maximumBy (compare `on` (manhattan . acceleration . snd)) ips
   in fst highestAccel

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
