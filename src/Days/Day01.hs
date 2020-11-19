module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
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
inputParser = many1 (digit >>= return . read . pure)

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Rotates a list by a given number
shift :: Int -> [Int] -> [Int]
shift n list = (drop n list) ++ (L.take n list)

-- Calculates the captchaScore of a pair of numbers
captchaScore :: Int -> Int -> Int
captchaScore a b = if a == b then a else 0

partA :: Input -> OutputA
partA input =
  foldr1 (+) $
    zipWith captchaScore input (shift 1 input)

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  foldr1 (+) $
    zipWith captchaScore input (shift (length input `div` 2) input)

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
