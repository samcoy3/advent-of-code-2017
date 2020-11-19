module Days.Day07 (runDay) where

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
import Data.Tree (Tree (..))
import qualified Data.Tree as Tree
import Data.Void
{- ORMOLU_ENABLE -}

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  name <- many1 letter
  asciiCI " ("
  weight <- decimal
  asciiCI ")"
  children <- option [] $ do
    asciiCI " -> "
    (many1 letter) `sepBy` asciiCI ", "
  insertProcessAndEdges (name, weight) children
    <$> option (ProcessTree Map.empty Map.empty) (endOfLine >> inputParser)

------------ TYPES ------------
data ProcessTree = ProcessTree
  { processWeights :: Map String Int,
    processDependencies :: Map String String
  }

-- Given one line of the problem input, combines it with an existing ProcessTree
insertProcessAndEdges :: (String, Int) -> [String] -> ProcessTree -> ProcessTree
insertProcessAndEdges (name, weight) children ProcessTree {..} =
  ProcessTree
    { processWeights =
        Map.insert name weight processWeights,
      processDependencies =
        foldr (uncurry Map.insert) processDependencies (zip children (repeat name))
    }

type Input = ProcessTree

type OutputA = String

type OutputB = Int

------------ PART A ------------
-- This part assumes that the ProcessTree is in fact a tree
partA :: Input -> OutputA
partA ProcessTree {..} =
  let processes = Map.keys processWeights
   in fromJust $ find (\p -> not $ p `elem` Map.keys processDependencies) processes

------------ PART B ------------
-- This makes the ProocessTree, into a Data.Tree (a rose tree)
makeTree :: ProcessTree -> Tree Int
makeTree pt@ProcessTree {..} = Tree.unfoldTree unfolder (partA pt)
  where
    unfolder node =
      ( processWeights Map.! node,
        Map.keys (Map.filter (== node) processDependencies)
      )

-- This function is a bit complicated:
-- - It assumes that the tree is unbalanced, until a point where it suddenly is not
-- - At that point, it's the root of that subtree that is "the problem"
-- - Every time we recurse down a level, we pass down the "correct value" for the root of the subtree we're considering, in case that node is "the problem". If it is the problem, then we just return the value we've passed down
findMisbalance :: Tree Int -> Int
findMisbalance (Node _ []) = error "No findMisbalance"
findMisbalance (Node a trees) = findMisbalance' 0 (Node a trees)
  where
    findMisbalance' correctValue (Node _ trees) = case oddOneOut trees of
      Nothing -> correctValue
      Just (newCorrectValue, unbalancedTree) -> findMisbalance' newCorrectValue unbalancedTree
    -- Returns Nothing if all the subtrees are the same weight
    -- Otherwise, finds the "odd tree out", and returns a pair containing:
    -- - The correct value for the root of the "odd tree out", assuming the root itself is at fault
    -- - The "odd tree out" itself
    oddOneOut trees =
      let oddTree = filter (\t -> not $ sumOfTree t `elem` (fmap sumOfTree $ delete t trees)) trees
       in case oddTree of
            [] -> Nothing
            (x@(Node val _) : []) ->
              Just $
                ( val - (sumOfTree x) + (sumOfTree $ head (delete x trees)),
                  x
                )
            _ -> error "Two odd ones out? What is this?"
    -- Sums the nodes in a tree using a fold
    sumOfTree = Tree.foldTree (\r ls -> sum $ r : ls)

partB :: Input -> OutputB
partB = findMisbalance . makeTree

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
