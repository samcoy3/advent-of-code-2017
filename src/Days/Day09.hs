module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Data.Attoparsec.Text as A
import Data.Text (pack)
import qualified Data.Text as Text (length)
import Data.Void
{- ORMOLU_ENABLE -}

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
-- Parses the top-level structure, where we might have several groups that aren't nested in each other
inputParser :: Parser Input
inputParser = do
  -- We skip over all the characters that aren't part of a group and aren't escape characters or garbage
  A.takeWhile (not . inClass "!<{")
  -- Now we do one of the following:
  choice
    -- Skip over an escape character and the following one (and start the input parser again)
    [ cancelledParser >> inputParser,
      -- Parse some garbage
      garbageParser >> inputParser,
      -- Or we find a group (whose level is 1, as this is the top level), and we parse it and attach it to the input parser recursively
      (:) <$> (groupParser 1) <*> inputParser,
      -- This is our empty list, for when there's no characters left
      return []
    ]

-- Parses a group, of level n
groupParser :: GroupLevel -> Parser Group
groupParser n = do
  char '{'
  takeRestOfGroup n
  where
    takeRestOfGroup n = do
      -- We ignore all the group filling that we don't care about
      A.takeWhile (not . inClass "!<{}")
      -- Then we either:
      choice
        -- See an escape sequence
        [ cancelledParser >> takeRestOfGroup n,
          -- See some garbage
          garbageParser >>= (\g -> addGarbage g <$> takeRestOfGroup n),
          -- See the end of the group
          char '}' >> (return $ Group n 0 []),
          -- Or see a subgroup (that is, we see a '{', and recurse)
          groupParser (n + 1) >>= (\g -> attachGroup g <$> (takeRestOfGroup n))
        ]

-- Parses some garbage
-- This is an 'Int' parser, because we need this for Part B
garbageParser :: Parser Int
garbageParser = do
  char '<'
  takeRestOfGarbage
  where
    takeRestOfGarbage = do
      -- This time we bind the stuff we don't care about, because we need to know its length
      garbage <- A.takeWhile (not . inClass "!>")
      choice
        -- Either the garbage ends...
        [ char '>' >> (return $ Text.length garbage),
          -- ...or we see an escape sequence (which doesn't contribute to length, as per Part B's rules)
          cancelledParser >> (+ (Text.length garbage)) <$> takeRestOfGarbage
        ]

-- This just sees a '!', and ignores both it and the following character
cancelledParser :: Parser ()
cancelledParser = char '!' >> anyChar >> return ()

------------ TYPES ------------
-- A group is a level, a quantity of garbage, and zero or more nested subgroups
type GroupLevel = Int

type GarbageQuantity = Int

data Group = Group GroupLevel GarbageQuantity [Group] deriving (Show)

-- We use this function to add a group as a "sub-group" of another group
attachGroup :: Group -> Group -> Group
attachGroup sub (Group l q rest) = Group l q (sub : rest)

-- We use this function to add garbage to a group's garbage count
addGarbage :: Int -> Group -> Group
addGarbage n (Group l g gs) = Group l (g + n) gs

-- Recursively sum the levels in a group
sumLevels :: Group -> Int
sumLevels (Group l _ []) = l
sumLevels (Group l q (g : gs)) = (sumLevels g) + (sumLevels (Group l q gs))

-- Recursively quantify the garbage in a group
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
