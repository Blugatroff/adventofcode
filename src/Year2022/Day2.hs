module Year2022.Day2 (partOne, partTwo) where

import Data.Char (isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Util (split, splitOnce, trim)

data Shape = Rock | Paper | Scissor
  deriving (Show, Eq, Ord)

data Outcome = Win | Lose | Draw
  deriving (Show, Eq)

parseShape :: String -> Either String Shape
parseShape "A" = Right Rock
parseShape "B" = Right Paper
parseShape "C" = Right Scissor
parseShape "X" = Right Rock
parseShape "Y" = Right Paper
parseShape "Z" = Right Scissor
parseShape s = Left $ "Failed to parse " <> s <> " as Rock Paper or Scissor"

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissor = 3

outcomeScore :: Outcome -> Int
outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore Lose = 0

parseLine :: String -> Either String (Shape, Shape)
parseLine line = case splitOnce ' ' line of
  Just (other, me) -> do
    other <- parseShape other
    me <- parseShape me
    return (other, me)
  Nothing -> Left $ "expected game got " <> line

parse :: String -> Either String [(Shape, Shape)]
parse input = split '\n' input <&> trim isSpace & traverse parseLine

logic :: M.Map (Shape, Shape) Outcome
logic =
  M.fromList
    [ ((Rock, Rock), Draw),
      ((Rock, Paper), Win),
      ((Rock, Scissor), Lose),
      ((Paper, Rock), Lose),
      ((Paper, Paper), Draw),
      ((Paper, Scissor), Win),
      ((Scissor, Rock), Win),
      ((Scissor, Paper), Lose),
      ((Scissor, Scissor), Draw)
    ]

play :: (Shape, Shape) -> Outcome
play game = M.lookup game logic & fromMaybe Win

solvePartOne :: [(Shape, Shape)] -> Int
solvePartOne lines = map gameScore lines & sum
  where
    gameScore :: (Shape, Shape) -> Int
    gameScore (a, b) = shapeScore b + outcomeScore (play (a, b))

chooseShapeFromOutcome :: Outcome -> Shape -> Shape
chooseShapeFromOutcome Draw s = s
chooseShapeFromOutcome Win Rock = Paper
chooseShapeFromOutcome Win Paper = Scissor
chooseShapeFromOutcome Win Scissor = Rock
chooseShapeFromOutcome Lose Rock = Scissor
chooseShapeFromOutcome Lose Paper = Rock
chooseShapeFromOutcome Lose Scissor = Paper

shapeToOutcome :: Shape -> Outcome
shapeToOutcome Rock = Lose
shapeToOutcome Paper = Draw
shapeToOutcome Scissor = Win

solvePartTwo :: [(Shape, Shape)] -> Int
solvePartTwo lines = map gameScore lines & sum
  where
    gameScore :: (Shape, Shape) -> Int
    gameScore (a, b) = score
      where
        chosen = chooseShapeFromOutcome desiredOutcome a
        desiredOutcome = shapeToOutcome b
        outcome = play (a, chosen)
        score = shapeScore chosen + outcomeScore outcome

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
