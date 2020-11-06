module Days.Day04 (runDay) where

import Data.Attoparsec.Text
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Void

------------ DAY LOGIC ------------
runDay :: IO ()
runDay = do
  input <- readFile "input/Day04.txt" >>= (return . parseOnly inputParser . pack)
  processInput input
  where
    processInput (Left x) = error x
    processInput (Right i) = do
      putStrLn "Part A:"
      print $ partA i
      putStrLn "Part B:"
      print $ partB i

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
passPhraseUnique :: [String] -> Bool
passPhraseUnique [] = True
passPhraseUnique (s : ss) =
  not (s `elem` ss)
    && passPhraseUnique ss

partA :: Input -> OutputA
partA =
  foldr
    (\p a -> if passPhraseUnique p then a + 1 else a)
    0

------------ PART B ------------
passPhraseAnagram :: [String] -> Bool
passPhraseAnagram [] = True
passPhraseAnagram (s : ss) =
  (not $ any (\word -> sort word == sort s) ss)
    && passPhraseAnagram ss

partB :: Input -> OutputB
partB =
  foldr
    (\p a -> if passPhraseAnagram p then a + 1 else a)
    0
