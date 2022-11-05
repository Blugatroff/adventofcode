module Days.Day14 (partOne, partTwo) where

import Control.Monad (forM_)
import Control.Monad.State (State, execState, get, modify)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find, foldl', sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Read (readEither)
import Util
  ( applyN,
    dedup,
    listToTuple,
    mapFst,
    mapSnd,
    maximumByKey,
    maybeToRight,
    minimumByKey,
    split,
    splitSeq,
    trim,
  )

type Rule = (Char, Char, Char)

data Input = Input
  { template :: !String,
    rules :: ![Rule]
  }
  deriving (Show)

parseRule :: String -> Either String Rule
parseRule line = case splitSeq "->" line <&> trim isSpace & concat of
  [left, right, between] -> Right (left, right, between)
  splits -> Left $ "failed to parse line: " <> line

parse :: String -> Either String Input
parse input = do
  case split '\n' input <&> trim isSpace & filter (not . null) of
    (template : rules) -> traverse parseRule rules <&> Input template
    _ -> Left $ "failed to parse: " <> input

windows :: Int -> [a] -> [[a]]
windows size [] = []
windows size list = take size list : windows size (tail list)

findRule :: String -> [Rule] -> Maybe Rule
findRule [left, right] = find (\(l, r, _) -> l == left && r == right)
findRule template = const Nothing

applyRules :: [Rule] -> String -> String
applyRules rules template = foldl' fold "" $ windows 2 template
  where
    fold :: String -> String -> String
    fold out window =
      out <> case findRule window rules of
        Just (left, right, between) -> take 1 window <> [between]
        Nothing -> take 1 window

computeScore :: String -> Either String Int
computeScore template = do
  let error = "cannot compute score of empty template"
  mostCommon <- dedup template & maximumByKey snd & maybeToRight error <&> snd
  leastCommon <- dedup template & minimumByKey snd & maybeToRight error <&> snd
  Right $ mostCommon - leastCommon

solvePartOne :: Int -> Input -> Either String Int
solvePartOne n input = applyN n (applyRules (rules input)) (template input) & computeScore

type Pair = (Char, Char)

type Pairs = M.Map Pair Int

type Letters = M.Map Char Int

getPairs :: String -> Pairs
getPairs = windows 2 <&> mapMaybe listToTuple <&> dedup <&> M.fromList

applyRule :: Rule -> Pairs -> State (Pairs, Letters) ()
applyRule (left, right, between) pairs = case M.lookup key pairs of
  Nothing -> return ()
  Just 0 -> return ()
  Just count -> do
    modify $ mapFst $ M.alter (Just . (+ (- count)) . fromMaybe 0) key
    modify $ mapFst $ M.alter (Just . (+ count) . fromMaybe 0) leftSide
    modify $ mapFst $ M.alter (Just . (+ count) . fromMaybe 0) rightSide
    modify $ mapSnd $ M.alter (Just . (+ count) . fromMaybe 0) between
  where
    key = (left, right)
    leftSide = (left, between)
    rightSide = (between, right)

applyRulespartTwo :: [Rule] -> State (Pairs, Letters) ()
applyRulespartTwo rules = do
  oldPairs <- get <&> fst
  forM_ rules $ \rule -> do
    applyRule rule oldPairs
    return ()

computeScorePartTwo :: Letters -> Int
computeScorePartTwo letters = last list - head list
  where
    list = sort $ M.toList letters <&> snd

runSteps :: Int -> [Rule] -> State (Pairs, Letters) ()
runSteps n rules = forM_ [1 .. n] $ \_ -> applyRulespartTwo rules

solvePartTwo :: Int -> Input -> Int
solvePartTwo n input =
  execState (runSteps n (rules input)) initialState
    & computeScorePartTwo . snd
  where
    initialState = (getPairs (template input), M.fromList $ dedup (template input))

partOne :: String -> Either String String
partOne input = parse input >>= solvePartOne 10 <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo 40 <&> show
