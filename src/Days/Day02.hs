module Days.Day02 (runDay) where

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
  (decimal `sepBy1` (many1 $ char '\t'))
    `sepBy1` endOfLine

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . (fmap rowChecksum)
  where
    rowChecksum row = (maximum row) - (minimum row)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . (fmap rowChecksum)
  where
    -- Finds an evenly dividing pair and performs the division
    rowChecksum [] = error "No evenly-dividing pair!"
    rowChecksum (x : xs) = case findMatch x xs of
      Just n -> n
      Nothing -> rowChecksum xs
    findMatch x [] = Nothing
    findMatch x (y : ys) =
      if
          | x `mod` y == 0 -> Just $ x `div` y
          | y `mod` x == 0 -> Just $ y `div` x
          | otherwise -> findMatch x ys

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
