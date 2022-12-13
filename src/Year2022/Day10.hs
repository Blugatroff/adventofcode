module Year2022.Day10 (partOne, partTwo) where

import Data.Char (isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl', intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Util (modifyList, readInt, split, splitOnce, trim)

data Instruction = AddX !Int | NoOp
  deriving (Show)

parseLine :: String -> Either String Instruction
parseLine "noop" = Right NoOp
parseLine line = case splitOnce ' ' line of
  Nothing -> Left $ "failed to parse line: " <> line
  Just ("addx", argument) -> readInt argument <&> AddX
  Just (instruction, argument) -> Left $ "unknown instruction: " <> instruction

parse :: String -> Either String [Instruction]
parse input = split '\n' input <&> trim isSpace & traverse parseLine

type DelayedOperation = Cpu -> Cpu

type PendingOperations = [[DelayedOperation]]

data Cpu = Cpu
  { xRegister :: !Int,
    cycleNumber :: !Int,
    pending :: !PendingOperations,
    pendingInstructions :: ![Instruction]
  }

modifyXRegister :: (Int -> Int) -> Cpu -> Cpu
modifyXRegister f cpu = cpu {xRegister = f $ xRegister cpu}

extendingModify :: a -> Int -> (a -> a) -> [a] -> [a]
extendingModify def index f list = take (max (index + 1) (length list)) $ modifyList index f $ list ++ repeat def

addOperation :: Int -> (Cpu -> Cpu) -> Cpu -> Cpu
addOperation delay operation cpu = cpu {pending = pending cpu & extendingModify [] delay (operation :)}

increaseCycleCounter :: Cpu -> Cpu
increaseCycleCounter cpu = cpu {cycleNumber = 1 + cycleNumber cpu}

executeCycle :: Cpu -> Maybe Cpu
executeCycle cpu =
  increaseCycleCounter <$> case pending cpu of
    (operationsThisCycle : nextOperations) ->
      applyPendingOperations operationsThisCycle nextOperations
    [] -> loadInstruction cpu
  where
    setPending :: PendingOperations -> Cpu -> Cpu
    setPending pending cpu = cpu {pending = pending}

    setPendingInstructions :: [Instruction] -> Cpu -> Cpu
    setPendingInstructions instructions cpu = cpu {pendingInstructions = instructions}

    applyPendingOperations operations next =
      if null next
        then Just $ fromMaybe newCpu $ loadInstruction newCpu
        else Just newCpu
      where
        newCpu = setPending next $ foldl' (&) cpu operations

    loadInstruction cpu = case pendingInstructions cpu of
      [] -> Nothing
      (instruction : nextInstructions) -> Just $
        setPendingInstructions nextInstructions $ case instruction of
          NoOp -> addOperation 0 id cpu
          (AddX v) -> addOperation 1 (modifyXRegister (+ v)) cpu

cpuStartState :: [Instruction] -> Cpu
cpuStartState = Cpu 1 0 []

samplePoints :: [Int]
samplePoints = [20, 60, 100, 140, 180, 220]

runCpu :: (Cpu -> a -> a) -> a -> Cpu -> a
runCpu f state cpu = case executeCycle cpu of
  Just newCpu -> runCpu f (f newCpu state) newCpu
  Nothing -> state

solvePartOne :: [Instruction] -> Int
solvePartOne instructions = sum $ runCpu sample [] (cpuStartState instructions)
  where
    sample cpu samples =
      if cycleNumber cpu `elem` samplePoints
        then xRegister cpu * cycleNumber cpu : samples
        else samples

type Crt = M.Map (Int, Int) Bool

crtWidth :: Int
crtWidth = 40

crtHeight :: Int
crtHeight = 6

drawCrt :: Crt -> String
drawCrt crt =
  intercalate "\n" $
    [0 .. crtHeight - 1] <&> \y ->
      [0 .. crtWidth - 1] <&> \x ->
        case M.lookup (x, y) crt of
          Nothing -> '.'
          Just True -> '#'
          Just False -> '.'

solvePartTwo :: [Instruction] -> String
solvePartTwo instructions = runCpu sample M.empty (cpuStartState instructions) & drawCrt
  where
    sample cpu crt = M.insert (x, y) value crt
      where
        x = (cycleNumber cpu - 1) `mod` crtWidth
        y = (cycleNumber cpu - 1) `div` crtWidth
        value = x >= xRegister cpu - 1 && x <= xRegister cpu + 1

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo
