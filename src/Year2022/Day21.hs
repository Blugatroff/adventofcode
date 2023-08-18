module Year2022.Day21 (partOne, partTwo) where

import Data.Either.Extra (maybeToEither)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Extra (firstJust)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map qualified as Map
import Util (splitOnce, trimSpace, readInteger)

data Operation = Add | Sub | Mul | Div
  deriving (Eq)

operationOperator :: Operation -> Char
operationOperator Add = '+'
operationOperator Sub = '-'
operationOperator Mul = '*'
operationOperator Div = '/'

runOperation :: Operation -> Integer -> Integer -> Integer
runOperation Add = (+)
runOperation Sub = (-)
runOperation Mul = (*)
runOperation Div = div

data Job = Yell !Integer | Calculate !Operation !String !String

data Monkey = Monkey !String !Job

parse :: String -> Either String (Map String Monkey)
parse input =
  lines input
    & filter (not . null)
    & traverse parseMonkey
    & fmap (Map.fromList . fmap (\m@(Monkey name _) -> (name, m)))

parseMonkey :: String -> Either String Monkey
parseMonkey line = do
  (name, job) <- splitOnce ':' line & maybeToEither ("Failed to parse line: " <> line)
  job <- parseJob job
  Right $ Monkey name job

parseJob :: String -> Either String Job
parseJob job = case firstJust (\o -> (o,) <$> splitOnce (operationOperator o) job) [Add, Sub, Mul, Div] of
  Just (o, (l, r)) -> Right $ Calculate o (trimSpace l) (trimSpace r)
  Nothing -> fmap Yell $ readInteger $ trimSpace job

solvePartOne :: Map String Monkey -> Maybe Integer
solvePartOne monkeys = evaluate =<< calculationFromMonkeys monkeys "root"

data Calculation = Constant String Integer | Calculation String Operation Calculation Calculation | Humn

calculationName :: Calculation -> Maybe String
calculationName Humn = Nothing
calculationName (Constant name _) = Just name
calculationName (Calculation name _ _ _) = Just name

replace :: (Calculation -> Bool) -> Calculation -> Calculation -> Calculation
replace f replacement calc | f calc = replacement
replace f replacement (Calculation name op l r) = Calculation name op (replace f replacement l) (replace f replacement r)
replace _ _ (Constant name value) = Constant name value
replace _ _ Humn = Humn

replaceWithHumn :: String -> Calculation -> Calculation
replaceWithHumn name = replace ((Just name ==) . calculationName) Humn

instance Eq Calculation where
  Humn == Humn = True
  (Constant _ l) == (Constant _ r) = l == r
  (Calculation _ lop ll lr) == (Calculation _ rop rl rr) = lop == rop && ll == rl && lr == rr
  _ == _ = False

calculationContainsHumn :: Calculation -> Bool
calculationContainsHumn Humn = True
calculationContainsHumn (Constant _ _) = False
calculationContainsHumn (Calculation _ _ l r) = calculationContainsHumn l || calculationContainsHumn r

calculationFromMonkeys :: Map String Monkey -> String -> Maybe Calculation
calculationFromMonkeys monkeys name = do
  Monkey _ job <- M.lookup name monkeys
  case job of
    Yell n -> Just $ Constant name n
    Calculate op l r -> do
      l <- calculationFromMonkeys monkeys l
      r <- calculationFromMonkeys monkeys r
      Just $ Calculation name op l r

evaluate :: Calculation -> Maybe Integer
evaluate (Constant _ n) = Just n
evaluate (Calculation _ op l r) = evaluate l >>= \l -> evaluate r <&> runOperation op l
evaluate Humn = Nothing

peelCalculation :: Calculation -> Calculation -> Calculation
peelCalculation (Calculation name Add l r) | calculationContainsHumn l = peelCalculation l . \rhs -> Calculation name Sub rhs r
peelCalculation (Calculation name Add l r) | calculationContainsHumn r = peelCalculation r . \rhs -> Calculation name Sub rhs l
peelCalculation (Calculation name Sub l r) | calculationContainsHumn l = peelCalculation l . Calculation name Add r
peelCalculation (Calculation name Sub l r) | calculationContainsHumn r = peelCalculation r . Calculation name Sub l
peelCalculation (Calculation name Div l r) | calculationContainsHumn l = peelCalculation l . Calculation name Mul r
peelCalculation (Calculation name Div l r) | calculationContainsHumn r = peelCalculation r . Calculation name Div l
peelCalculation (Calculation name Mul l r) | calculationContainsHumn l = peelCalculation l . \rhs -> Calculation name Div rhs r
peelCalculation (Calculation name Mul l r) | calculationContainsHumn r = peelCalculation r . \rhs -> Calculation name Div rhs l
peelCalculation (Calculation {}) = id
peelCalculation (Constant _ _) = id
peelCalculation Humn = id

solvePartTwo :: Map String Monkey -> Maybe Integer
solvePartTwo monkeys = do
  Monkey _ job <- M.lookup "root" monkeys
  (_, (l, r)) <- case job of
    Yell _ -> Nothing
    Calculate op l r -> Just (op, (l, r))
  lc <- replaceWithHumn "humn" <$> calculationFromMonkeys monkeys l
  rc <- replaceWithHumn "humn" <$> calculationFromMonkeys monkeys r
  (humn, constant) <- case (calculationContainsHumn lc, calculationContainsHumn rc) of
    (True, True) -> Nothing
    (False, False) -> Nothing
    (True, False) -> Just (lc, rc)
    (False, True) -> Just (rc, lc)
  evaluate $ peelCalculation humn constant

partOne :: String -> Either String String
partOne = fmap (show . solvePartOne) <$> parse

partTwo :: String -> Either String String
partTwo = fmap (show . solvePartTwo) <$> parse
