module Year2022.Day5 (partOne, partTwo) where

import Data.Char (isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl', transpose)
import qualified Data.Map.Strict as M
import Util (readInt, replace, split, splitOnce, trim)

transposeStacks :: [String] -> [String]
transposeStacks input =
  transpose input
    <&> reverse
    <&> filter (/= ']')
    <&> filter (/= '[')
    <&> trim isSpace
    & filter (not . null)

parseStacks :: [String] -> [Stack]
parseStacks lines = lines <&> drop 1 <&> reverse

parseMove :: String -> Either String Move
parseMove line =
  line
    & replace "move" ""
    & replace "from" ""
    & replace "to" ""
    & split ' '
    & filter (not . null)
    & traverse readInt
    >>= \case
      [stack, source, destination] -> Right (Move stack source destination)
      notThreeInts -> Left $ "Failed to parse move from line: " <> line

parseMoves :: [String] -> Either String [Move]
parseMoves lines = lines & filter (not . null) & traverse parseMove

data Move = Move
  { amount :: !Int,
    source :: !Int,
    destination :: !Int
  }
  deriving (Show)

type Stack = [Char]

parse :: String -> Either String (M.Map Int Stack, [Move])
parse input = do
  let lines = split '\n' input
  case splitOnce "" lines of
    Nothing -> Left "failed to find separation between stacks and moves"
    Just (stacks, moves) -> do
      moves <- parseMoves moves
      let stackMap = M.fromList $ zip [1 ..] $ parseStacks $ transposeStacks stacks
      return (stackMap, moves)

applyMove :: ([Char] -> [Char]) -> M.Map Int Stack -> Move -> M.Map Int Stack
applyMove reverseOrNot stacks move = case (M.lookup (source move) stacks, M.lookup (destination move) stacks) of
  (Just srcStack, Just destStack) ->
    M.insert (destination move) (reverseOrNot (take (amount move) srcStack) ++ destStack) stacks
      & M.insert (source move) (drop (amount move) srcStack)
  _ -> stacks

onTop :: M.Map Int Stack -> String
onTop stacks = M.elems stacks >>= take 1

solve :: ([Char] -> [Char]) -> (M.Map Int Stack, [Move]) -> String
solve reverseOrNot (stacks, moves) = output ++ onTop outputStacks
  where
    (_, output, outputStacks) = foldl' folder (0, "", stacks) moves

    numberMoves = length moves

    folder :: (Int, String, M.Map Int [Char]) -> Move -> (Int, String, M.Map Int [Char])
    folder (i, output, stacks) move = (i + 1, output ++ message, applyMove reverseOrNot stacks move)
      where
        message = show i <> "/" <> show numberMoves <> "\n"

solvePartOne :: (M.Map Int Stack, [Move]) -> String
solvePartOne = solve reverse

solvePartTwo :: (M.Map Int Stack, [Move]) -> String
solvePartTwo = solve id

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo
