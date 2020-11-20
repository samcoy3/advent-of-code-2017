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

import System.Directory (doesFileExist)
import Control.Exception (catch, SomeException)
import Control.Monad.Except
import Data.Attoparsec.Text
import Data.Text (pack)
import Data.Void
{- ORMOLU_ENABLE -}

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
  deriving (Eq, Ord)

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
