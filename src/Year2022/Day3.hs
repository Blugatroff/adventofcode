module Year2022.Day3 (partOne, partTwo) where

import Data.Char (isAsciiLower, isAsciiUpper, isSpace, ord)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Util (chunks, reduceR, split, trim)
import qualified Util

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
solvePartOne rucksacks =
  rucksacks
    <&> (\(a, b) -> [a, b])
    <&> findIntersection & concat
    <&> priority & sum

solvePartTwo :: [[String]] -> Int
solvePartTwo rucksacks =
  rucksacks
    <&> findIntersection & concat
    <&> priority & sum

partOne :: String -> Either String String
partOne input = parsePartOne input & solvePartOne & show & Right

partTwo :: String -> Either String String
partTwo input = parsePartTwo input & solvePartTwo & show & Right
