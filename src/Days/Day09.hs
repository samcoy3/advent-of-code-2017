module Days.Day09 where

import Data.Attoparsec.Text as A
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Text as Text (length)
import Data.Void

------------ DAY LOGIC ------------
runDay :: IO ()
runDay = do
  input <- readFile "input/Day09.txt" >>= (return . parseOnly inputParser . pack)
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
  A.takeWhile (not . inClass "!<{")
  choice
    [ cancelledParser >> inputParser,
      garbageParser >> inputParser,
      (:) <$> (groupParser 1) <*> inputParser,
      return []
    ]

groupParser :: Int -> Parser Group
groupParser n = do
  char '{'
  takeRestOfGroup n
  where
    takeRestOfGroup n = do
      A.takeWhile (not . inClass "!<{}")
      choice
        [ cancelledParser >> takeRestOfGroup n,
          garbageParser >>= (\g -> addGarbage g <$> takeRestOfGroup n),
          char '}' >> (return $ Group n 0 []),
          groupParser (n + 1) >>= (\g -> attachGroup g <$> (takeRestOfGroup n))
        ]

garbageParser :: Parser Int
garbageParser = do
  char '<'
  takeRestOfGarbage
  where
    takeRestOfGarbage = do
      garbage <- A.takeWhile (not . inClass "!>")
      choice
        [ char '>' >> (return $ Text.length garbage),
          cancelledParser >> (+ (Text.length garbage)) <$> takeRestOfGarbage
        ]

cancelledParser :: Parser ()
cancelledParser = char '!' >> anyChar >> return ()

------------ TYPES ------------
type GroupLevel = Int

type GarbageQuantity = Int

data Group = Group GroupLevel GarbageQuantity [Group] deriving (Show)

attachGroup :: Group -> Group -> Group
attachGroup sub (Group l q rest) = Group l q (sub : rest)

addGarbage :: Int -> Group -> Group
addGarbage n (Group l g gs) = Group l (g + n) gs

sumLevels :: Group -> Int
sumLevels (Group l _ []) = l
sumLevels (Group l q (g : gs)) = (sumLevels g) + (sumLevels (Group l q gs))

sumGarbage :: Group -> Int
sumGarbage (Group _ q []) = q
sumGarbage (Group l q (g : gs)) = (sumGarbage g) + (sumGarbage (Group l q gs))

type Input = [Group]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . fmap sumLevels

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . fmap sumGarbage
