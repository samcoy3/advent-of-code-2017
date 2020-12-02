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
          string "rcv" >> (Rcv <$> letter'),
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
  | Rcv Char
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

type OutputB = Int

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
  v <- getValue (Left x)
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
data NMachine = NMachine
  { registers' :: Map Char Int,
    inQueue :: [Int],
    currentPos :: Int,
    sendCount :: Int
  }
  deriving (Show)

type MachinePair = (NMachine, NMachine)

type Duet2 = State MachinePair

data MachineNumber = Zero | One

type Blocked = Bool

getValue' :: MachineNumber -> Value -> Duet2 Int
getValue' f v = do
  let accessor = case f of
        Zero -> fst
        One -> snd
  rs <- gets (registers' . accessor)
  return $ case v of
    Left c -> Map.findWithDefault 0 c rs
    Right i -> i

modifyPair :: MachineNumber -> (NMachine -> NMachine) -> Duet2 Bool
modifyPair Zero s = modify (\(a, b) -> (s a, b)) >> return False
modifyPair One s = modify (\(a, b) -> (a, s b)) >> return False

getAsPair :: (NMachine -> a) -> Duet2 (a, a)
getAsPair f = (,) <$> gets (f . fst) <*> gets (f . snd)

getsPair :: MachineNumber -> (NMachine -> a) -> Duet2 a
getsPair Zero f = gets (f . fst)
getsPair One f = gets (f . snd)

otherMachine :: MachineNumber -> MachineNumber
otherMachine Zero = One
otherMachine One = Zero

execB :: MachineNumber -> Instruction -> Duet2 Blocked
execB f (Snd x) = do
  v <- getValue' f x
  modifyPair (otherMachine f) (\m@NMachine {..} -> m {inQueue = inQueue ++ [v]})
  modifyPair f (\m@NMachine {..} -> m {sendCount = sendCount + 1})
execB f (Set x y) = do
  v <- getValue' f y
  modifyPair f (\m@NMachine {..} -> m {registers' = Map.insert x v registers'})
execB f (Add x y) = do
  v <- getValue' f y
  modifyPair f (\m@NMachine {..} -> m {registers' = Map.adjust (+ v) x registers'})
execB f (Mul x y) = do
  v <- getValue' f y
  modifyPair f (\m@NMachine {..} -> m {registers' = Map.adjust (* v) x registers'})
execB f (Mod x y) = do
  v <- getValue' f y
  modifyPair f (\m@NMachine {..} -> m {registers' = Map.adjust (`mod` v) x registers'})
execB f (Rcv x) = do
  canConsume <- getsPair f (not . null . inQueue)
  case canConsume of
    True -> do
      modifyPair
        f
        ( \m@NMachine {..} ->
            m
              { inQueue = tail inQueue,
                registers' = Map.insert x (head inQueue) registers'
              }
        )
    False -> modifyPair f (\m@NMachine {..} -> m {currentPos = currentPos - 1}) >> return True
execB f (Jgz x y) = do
  v <- getValue' f x
  w <- getValue' f y
  when (v > 0) $
    modifyPair f (\m@NMachine {..} -> m {currentPos = currentPos + (w - 1)}) >> return ()
  return False

execProgramB :: Vector Instruction -> Duet2 Int
execProgramB v = do
  (i0, i1) <- getAsPair currentPos
  b0 <-
    if i0 < 0 || i0 >= (Vec.length v)
      then return True
      else do
        (execB Zero (v Vec.! i0))
          <* (modifyPair Zero (\m@NMachine {..} -> m {currentPos = currentPos + 1}))
  b1 <-
    if i1 < 0 || i1 >= (Vec.length v)
      then return True
      else
        (execB One (v Vec.! i1))
          <* (modifyPair One (\m@NMachine {..} -> m {currentPos = currentPos + 1}))
  if b0 && b1
    then getsPair One sendCount
    else execProgramB v

-- partB :: Input -> OutputB
partB = flip evalState startingState . execProgramB
  where
    startingState =
      ( NMachine
          { registers' = Map.singleton 'p' 0,
            currentPos = 0,
            sendCount = 0,
            inQueue = []
          },
        NMachine
          { registers' = Map.singleton 'p' 1,
            currentPos = 0,
            sendCount = 0,
            inQueue = []
          }
      )
