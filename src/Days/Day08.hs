module Days.Day08 (runDay) where

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
inputParser = parseInstruction `sepBy` endOfLine

parseInstruction :: Parser Instruction
parseInstruction = do
  mr <- many1 letter
  space
  mFunc <-
    choice
      [ asciiCI "inc" >> return (+),
        asciiCI "dec" >> return (-)
      ]
  space
  mMod <- signed decimal
  asciiCI " if "
  cr <- many1 letter
  space
  cFunc <-
    choice
      [ asciiCI ">=" >> return (>=),
        asciiCI "==" >> return (==),
        asciiCI "<=" >> return (<=),
        asciiCI "!=" >> return (/=),
        asciiCI ">" >> return (>),
        asciiCI "<" >> return (<)
      ]
  space
  cVal <- signed decimal
  return
    Instruction
      { condition =
          Condition
            { cRegister = cr,
              cCondition = (flip cFunc) cVal
            },
        modification =
          Modification
            { mRegister = mr,
              mModification = (flip mFunc) mMod
            }
      }

------------ TYPES ------------
data Instruction = Instruction
  { condition :: Condition,
    modification :: Modification
  }

instance Show Instruction where
  show i = "Condition reg: " ++ (cRegister . condition $ i) ++ " , Modification reg: " ++ (mRegister . modification $ i)

data Condition = Condition
  { cRegister :: Register,
    cCondition :: (Int -> Bool)
  }

data Modification = Modification
  { mRegister :: Register,
    mModification :: Int -> Int
  }

type Register = String

type Input = [Instruction]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
executeInstruction :: Instruction -> Map String Int -> Map String Int
executeInstruction Instruction {..} m =
  let checkValue = Map.findWithDefault 0 (cRegister condition) m
      modifyValue = Map.findWithDefault 0 (mRegister modification) m
   in if (cCondition condition) checkValue
        then Map.insert (mRegister modification) (mModification modification $ modifyValue) m
        else m

partA :: Input -> OutputA
partA input =
  let finalRegisters = foldr1 (flip (.)) (fmap executeInstruction input) $ Map.empty
   in maximum . Map.elems $ finalRegisters

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let partialExecutions = scanl1 (flip (.)) (fmap executeInstruction input)
      finalRegisterValues = concat . fmap Map.elems . fmap (flip ($) Map.empty) $ partialExecutions
   in maximum finalRegisterValues

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
