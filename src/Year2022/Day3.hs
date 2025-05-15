module Year2022.Day3 (partOne, partTwo) where

import MeLude
import qualified Data.Set as S
import Util (chunks, reduceR, split, trim)

parsePartOne :: String -> [(String, String)]
parsePartOne input =
  split '\n' input
    <&> trim isSpace & filter (not . null)
    <&> \line -> splitAt (length line `div` 2) line

parsePartTwo :: String -> [[String]]
parsePartTwo input =
  split '\n' input
    <&> trim isSpace
    & filter (not . null)
    & chunks 3

findIntersection :: [String] -> [Char]
findIntersection lists =
  lists
    <&> S.fromList
    & reduceR S.intersection
    & maybe [] S.toList

priority :: Char -> Int
priority c
  | isAsciiLower c = ord c - ord 'a' + 1
  | isAsciiUpper c = (ord c - ord 'A') + 27
  | otherwise = 0

solvePartOne :: [(String, String)] -> Int
solvePartOne rucksacks = sum $ do
  (a, b) <- rucksacks
  priority <$> findIntersection [a, b]

solvePartTwo :: [[String]] -> Int
solvePartTwo rucksacks = sum $ do
  rucksack <- rucksacks
  priority <$> findIntersection rucksack

partOne :: String -> Either String String
partOne input = parsePartOne input & solvePartOne & show & Right

partTwo :: String -> Either String String
partTwo input = parsePartTwo input & solvePartTwo & show & Right
