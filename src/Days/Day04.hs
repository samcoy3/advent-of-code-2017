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

import System.Directory (doesFileExist)
import Control.Exception (catch, SomeException)
import Control.Monad.Except
import Data.Attoparsec.Text
import Data.Text (pack)
import Data.Void
{- ORMOLU_ENABLE -}

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

------------ DAY LOGIC ------------
runDay :: String -> IO ()
runDay inputFile = do
  input <- runExceptT $ do
    inputFileExists <- liftIO $ doesFileExist inputFile
    fileContents <-
      if inputFileExists
        then (liftIO $ readFile inputFile)
        else throwError $ "I couldn't read the input! I was expecting it to be at " ++ inputFile
    case (parseOnly inputParser . pack $ fileContents) of
      Left e -> throwError $ "Parser failed to read input. Error " ++ e
      Right i -> return i
  processInput input
  where
    processInput (Left x) = putStrLn x
    processInput (Right i) = do
      putStrLn "Part A:"
      catch (print $ partA i) (\m -> return (m :: SomeException) >> putStrLn "Couldn't run Part A!")
      putStrLn "Part B:"
      catch (print $ partB i) (\m -> return (m :: SomeException) >> putStrLn "Couldn't run Part B!")
