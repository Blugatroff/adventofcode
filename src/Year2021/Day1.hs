module Year2021.Day1 (partOne, partTwo) where

import MeLude
import qualified Util

parse :: String -> Either String [Int]
parse = traverse readEither . Util.split '\n'

groups :: [Int] -> [[Int]]
groups (a : b : c : rest) = [a, b, c] : groups (b : c : rest)
groups _ = []

solvePartOne :: [Int] -> Int
solvePartOne list = snd $ foldl' f (head list, 0) $ tail list
  where
    f (p, c) v | v > p = (v, c + 1)
    f (p, c) v = (v, c)

solvePartTwo :: [Int] -> Int
solvePartTwo = solvePartOne . map sum . groups

partOne :: String -> Either String String
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input
