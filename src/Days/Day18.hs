module Days.Day18 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void

import Control.Monad.State
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Vec.fromList <$> (instruction `sepBy` endOfLine)
  where
    value = space >> eitherP letter (signed decimal)
    letter' = space >> letter
    instruction =
      choice
        [ string "snd" >> (Snd <$> value),
          string "set" >> (Set <$> letter' <*> value),
          string "add" >> (Add <$> letter' <*> value),
          string "mul" >> (Mul <$> letter' <*> value),
          string "mod" >> (Mod <$> letter' <*> value),
          string "rcv" >> (Rcv <$> value),
          string "jgz" >> (Jgz <$> value <*> value)
        ]

------------ TYPES ------------
type Frequency = Int

type Duet = State Machine

type Value = Either Char Int

getValue :: Value -> Duet Int
getValue v = do
  rs <- gets registers
  return $ case v of
    Left c -> Map.findWithDefault 0 c rs
    Right i -> i

data Instruction
  = Snd Value
  | Set Char Value
  | Add Char Value
  | Mul Char Value
  | Mod Char Value
  | Rcv Value
  | Jgz Value Value
  deriving (Show)

data Machine = Machine
  { registers :: Map Char Int,
    instructionNum :: Int,
    lastSound :: Int,
    recoveredSounds :: [Int]
  }

type Input = Vector Instruction

type OutputA = Int

type OutputB = Void

------------ PART A ------------
exec :: Instruction -> Duet ()
exec (Snd x) = do
  v <- getValue x
  modify (\m -> m {lastSound = v})
exec (Set x y) = do
  v <- getValue y
  modify (\m@Machine {..} -> m {registers = Map.insert x v registers})
exec (Add x y) = do
  v <- getValue y
  modify (\m@Machine {..} -> m {registers = Map.adjust (+ v) x registers})
exec (Mul x y) = do
  v <- getValue y
  modify (\m@Machine {..} -> m {registers = Map.adjust (* v) x registers})
exec (Mod x y) = do
  v <- getValue y
  modify (\m@Machine {..} -> m {registers = Map.adjust (`mod` v) x registers})
exec (Rcv x) = do
  v <- getValue x
  when (v /= 0) $
    modify (\m@Machine {..} -> m {recoveredSounds = lastSound : recoveredSounds})
exec (Jgz x y) = do
  v <- getValue x
  w <- getValue y
  when (v > 0) $
    modify (\m@Machine {..} -> m {instructionNum = instructionNum + (w - 1)})

isRecover :: Instruction -> Bool
isRecover (Rcv _) = True
isRecover _ = False

execProgramA :: Vector Instruction -> Duet Int
execProgramA v = do
  currentInstruction <- (v Vec.!) <$> gets instructionNum
  exec currentInstruction
  modify (\m@Machine {..} -> m {instructionNum = instructionNum + 1})
  let isR = isRecover currentInstruction
  soundRecovered <- gets (not . null . recoveredSounds)
  if isR && soundRecovered
    then gets (head . recoveredSounds)
    else execProgramA v

partA :: Input -> OutputA
partA = flip evalState startingState . execProgramA
  where
    startingState =
      Machine
        { registers = Map.empty,
          instructionNum = 0,
          lastSound = 0,
          recoveredSounds = []
        }

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
