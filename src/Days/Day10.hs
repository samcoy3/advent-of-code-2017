module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Data.Bits
import Data.Char
import Text.Printf

import System.Directory (doesFileExist)
import Control.Exception (catch, SomeException)
import Control.Monad.Except
import Data.Attoparsec.Text
import Data.Text (pack)
import Data.Void
{- ORMOLU_ENABLE -}

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` char ','

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = String

------------ PART A ------------
-- Our initial list
standardList :: Vector Int
standardList = Vec.fromList [0 .. 255]

-- Reverses part of a vector, as per the question.
-- Supports index "wrapping"
reversePortion :: Int -> Int -> Vector a -> Vector a
reversePortion currentPosition len vec =
  let affectedIndices = fmap ((`mod` (Vec.length vec)) . (+ currentPosition)) [0 .. (len -1)]
      affectedValues = fmap (vec Vec.!) affectedIndices
   in (Vec.//) vec . zip affectedIndices $ reverse affectedValues

-- Performs one round of the hash -- all that is required for Part A
oneRound :: Vector a -> Int -> Int -> [Int] -> Vector a
oneRound vec _ _ [] = vec
oneRound vec currPos skipSize (l : ls) =
  oneRound
    (reversePortion currPos l vec)
    ((currPos + l + skipSize) `mod` Vec.length vec)
    (skipSize + 1)
    ls

partA :: Input -> OutputA
partA input =
  let resultVec = oneRound standardList 0 0 input
   in (resultVec Vec.! 0) * (resultVec Vec.! 1)

------------ PART B ------------
-- Converts the input into ASCII, and appends the required array
inputToAscii :: String -> [Int]
inputToAscii = (++ [17, 31, 73, 47, 23]) . fmap ord

-- Performs 64 rounds of the hash
sixtyFourRounds :: [Int] -> Vector a -> Vector a
sixtyFourRounds ls vec = sixtyFourRounds' vec 0 0 1 ls
  where
    sixtyFourRounds' vec _ _ 64 [] = vec
    sixtyFourRounds' vec c s r [] = sixtyFourRounds' vec c s (r + 1) ls
    sixtyFourRounds' vec c s r (l : ls) =
      sixtyFourRounds'
        (reversePortion c l vec)
        ((c + l + s) `mod` Vec.length vec)
        (s + 1)
        r
        ls

-- Densifies the hash by xor'ing 16-element chunks of the hash
densifyHash :: Vector Int -> [Int]
densifyHash = densifyHash' . Vec.toList
  where
    densifyHash' [] = []
    densifyHash' l = (foldr1 xor . L.take 16 $ l) : (densifyHash' . drop 16 $ l)

-- Converts the xor'ed parts of the hash to a length-2 hex string
hexify :: Int -> String
hexify = printf "%02x"

partB :: Input -> OutputB
partB = concat . fmap hexify . densifyHash . (flip sixtyFourRounds) standardList

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
      i' <- readFile inputFile
      putStrLn "Part B:"
      catch (print $ partB (inputToAscii . init $ i')) (\m -> return (m :: SomeException) >> putStrLn "Couldn't run Part B!")
