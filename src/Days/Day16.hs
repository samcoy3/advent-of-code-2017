module Days.Day16 (runDay) where

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
inputParser = move `sepBy` char ','
  where
    move = choice [spin, exchange, partner]
    spin = char 's' >> decimal >>= return . Spin
    exchange = do
      char 'x'
      a <- decimal
      char '/'
      b <- decimal
      return (Exchange a b)
    partner = do
      char 'p'
      a <- anyChar
      char '/'
      b <- anyChar
      return (Partner a b)

------------ TYPES ------------
data DanceMove
  = Spin Int
  | Exchange Int Int
  | Partner Char Char
  deriving (Show)

type Input = [DanceMove]

type OutputA = String

type OutputB = Void

------------ PART A ------------
perform :: DanceMove -> (Vector Char -> Vector Char)
perform (Spin n) = \v ->
  let (begin, end) = Vec.splitAt (16 - n) v
   in end Vec.++ begin
perform (Exchange a b) = \v ->
  v Vec.// [(a, v Vec.! b), (b, v Vec.! a)]
perform (Partner a b) = \v ->
  let aLoc = fromJust $ Vec.findIndex (== a) v
      bLoc = fromJust $ Vec.findIndex (== b) v
   in v Vec.// [(aLoc, b), (bLoc, a)]

partA :: Input -> OutputA
partA moves = Vec.toList . foldr1 (flip (.)) (fmap perform moves) . Vec.fromList $ ['a' .. 'p']

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
