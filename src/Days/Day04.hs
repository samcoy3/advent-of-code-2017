module Days.Day04 (runDay) where

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
inputParser =
  ((many1 letter) `sepBy1` (char ' '))
    `sepBy1` endOfLine

------------ TYPES ------------
type Input = [[String]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Recursively test whether a passphrase is valid
-- For part a, this involves checking to see if the phrase contained two of the same word.
passPhraseUnique :: [String] -> Bool
passPhraseUnique [] = True
passPhraseUnique (s : ss) =
  not (s `elem` ss)
    && passPhraseUnique ss

partA :: Input -> OutputA
partA = length . filter passPhraseUnique

------------ PART B ------------
-- Recursively tests whether a passphrase is valid
-- For part b, this checks whether any two words are anagrams of each other
passPhraseAnagram :: [String] -> Bool
passPhraseAnagram [] = True
passPhraseAnagram (s : ss) =
  (not $ any (\word -> sort word == sort s) ss)
    && passPhraseAnagram ss

partB :: Input -> OutputB
partB = length . filter passPhraseAnagram
