module Days.Day07 (runDay) where

import Data.Attoparsec.Text
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Tree (Tree (..))
import qualified Data.Tree as Tree
import Data.Void

------------ DAY LOGIC ------------
runDay :: IO ()
runDay = do
  input <- readFile "input/Day07.txt" >>= (return . parseOnly inputParser . pack)
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
partA :: Input -> OutputA
partA ProcessTree {..} =
  let processes = Map.keys processWeights
   in fromJust $ find (\p -> not $ p `elem` Map.keys processDependencies) processes

------------ PART B ------------
makeTree :: ProcessTree -> Tree Int
makeTree pt@ProcessTree {..} = Tree.unfoldTree unfolder (partA pt)
  where
    unfolder node =
      ( processWeights Map.! node,
        Map.keys (Map.filter (== node) processDependencies)
      )

findMisbalance :: Tree Int -> Int
findMisbalance (Node _ []) = error "No findMisbalance"
findMisbalance (Node a trees) = findMisbalance' 0 (Node a trees)
  where
    findMisbalance' correctValue (Node _ trees) = case oddOneOut trees of
      Nothing -> correctValue
      Just (newCorrectValue, unbalancedTree) -> findMisbalance' newCorrectValue unbalancedTree
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
    sumOfTree = Tree.foldTree (\r ls -> sum $ r : ls)

partB :: Input -> OutputB
partB = findMisbalance . makeTree
