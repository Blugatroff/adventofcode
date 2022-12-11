module Year2022.Day11 (partOne, partTwo) where

import Control.Monad.State (MonadState (get), State, execState, modify)
import Data.Char (isDigit, isSpace)
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Util (modifyList, readInt, split, splitOnce, splitSeq, trim)
import qualified Util

data OperationArgument = OldValue | Number !Int deriving (Show)

data Operation = Multiply !OperationArgument | Add !OperationArgument deriving (Show)

type Actions = (Int, Int)

data Monkey = Monkey
  { items :: ![Int],
    operation :: !Operation,
    test :: !Int,
    actions :: !Actions,
    inspectionCount :: !Int
  }
  deriving (Show)

parseMonkey :: String -> Either String Monkey
parseMonkey input = case split '\n' input <&> trim isSpace & filter (not . null) of
  [header, items, operation, test, ifTrue, ifFalse] -> do
    items <- parseItems items
    operation <- parseOperation operation
    test <- parseTest test
    ifTrue <- filter isDigit ifTrue & readInt
    ifFalse <- filter isDigit ifFalse & readInt
    return $ Monkey items operation test (ifTrue, ifFalse) 0
  monkey -> Left $ "failed to parse monkey:\n" <> input

parseItems :: String -> Either String [Int]
parseItems line = case splitOnce ':' line of
  Nothing -> Left $ "failed to parse items: " <> line
  Just (text, items) ->
    split ',' items
      <&> trim isSpace
      & filter (not . null)
      & traverse readInt

parseOperation :: String -> Either String Operation
parseOperation line =
  case (splitOnce '*' line, splitOnce '+' line) of
    (Nothing, Nothing) -> Left $ "failed to parse operation: " <> line
    (Just (_, right), _) -> case trim isSpace right of
      "old" -> Right $ Multiply OldValue
      number -> Multiply . Number <$> readInt (trim isSpace number)
    (_, Just (_, right)) -> case trim isSpace right of
      "old" -> Right $ Multiply OldValue
      number -> Add . Number <$> readInt (trim isSpace number)

parseTest :: String -> Either String Int
parseTest line = filter isDigit line & readInt

parse :: String -> Either String [Monkey]
parse input = splitSeq "\n\n" input & traverse parseMonkey

evaluateTest :: Int -> Int -> Bool
evaluateTest test n = n `mod` test == 0

modifyMonkeyItems :: ([Int] -> [Int]) -> Monkey -> Monkey
modifyMonkeyItems f monkey = monkey {items = f $ items monkey}

addToMonkeyItems :: Int -> Monkey -> Monkey
addToMonkeyItems item = modifyMonkeyItems (++ [item])

incrementInspectionCount :: Monkey -> Monkey
incrementInspectionCount monkey = monkey {inspectionCount = inspectionCount monkey + 1}

executeOperation :: Operation -> Int -> Int
executeOperation (Add (Number v)) n = n + v
executeOperation (Multiply (Number v)) n = n * v
executeOperation (Add OldValue) n = n + n
executeOperation (Multiply OldValue) n = n * n

playRound :: (Int -> Int) -> State [Monkey] ()
playRound f = do
  monkeys <- get
  forM_ [0 .. length monkeys - 1] $ \index -> do
    monkeys <- get
    let monkey = monkeys !! index
    forM_ (items monkey) $ \item -> do
      item <- pure $ executeOperation (operation monkey) item
      item <- pure $ f item
      let targetMonkey =
            (if evaluateTest (test monkey) item then fst else snd) $ actions monkey
      modify $ modifyList targetMonkey $ addToMonkeyItems item
      modify $ modifyList index incrementInspectionCount
    modify $ modifyList index $ modifyMonkeyItems $ const []

repeateState :: Int -> State s a -> State s [a]
repeateState 0 state = pure []
repeateState n state = do
  v <- state
  values <- repeateState (n - 1) state
  return (values ++ [v])

solvePartOne :: [Monkey] -> Int
solvePartOne monkeys =
  execState (repeateState 20 (playRound (`mod` 3))) monkeys
    <&> inspectionCount & sort & reverse & take 2 & product

solvePartTwo :: [Monkey] -> Int
solvePartTwo monkeys =
  execState (repeateState 10000 (playRound (`mod` modBase))) monkeys
    <&> inspectionCount & sort & reverse & take 2 & product
  where
    modBase = monkeys <&> test & product

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
