module Year2022.Day4 (partOne, partTwo) where

import MeLude
import Util (readInt, split, splitOnce, trim)
import Data.Range (Range(..))

parseRange :: String -> Either String (Range Int)
parseRange range = case splitOnce '-' range of
  Nothing -> Left $ "Failed to parse range: " <> range
  Just (left, right) -> do
    left <- readInt left
    right <- readInt right
    return (Range left right)

parseLine :: String -> Either String (Range Int, Range Int)
parseLine line = case splitOnce ',' line of
  Nothing -> Left $ "Failed to parse line " <> line
  Just (left, right) -> do
    left <- parseRange left
    right <- parseRange right
    return (left, right)

parse :: String -> Either String [(Range Int, Range Int)]
parse input =
  split '\n' input
    <&> trim isSpace
    & filter (not . null)
    & traverse parseLine

rangeContains :: Range Int -> Range Int -> Bool
rangeContains (Range ls le) (Range rs re) = ls <= rs && le >= re

rangesContainEachOther :: Range Int -> Range Int -> Bool
rangesContainEachOther a b = rangeContains a b || rangeContains b a

rangesOverlap :: Range Int -> Range Int -> Bool
rangesOverlap (Range ls le) (Range rs re) =
  (rs >= ls && rs <= le)
    || (re >= ls && re <= le)
    || (ls >= rs && ls <= re)
    || (le >= rs && le <= re)

solvePartOne :: [(Range Int, Range Int)] -> Int
solvePartOne lines =
  lines
    & filter (uncurry rangesContainEachOther)
    & length

solvePartTwo :: [(Range Int, Range Int)] -> Int
solvePartTwo lines =
  lines
    & filter (uncurry rangesOverlap)
    & length

partOne :: String -> Either String String
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input
