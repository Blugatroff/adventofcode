module Year2022.Day4 (partOne, partTwo) where

import Data.Char (isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intersperse, sort)
import Data.Maybe (fromMaybe)
import Util (readInt, split, splitOnce, trim)
import qualified Util

type Range = (Int, Int)

parseRange :: String -> Either String Range
parseRange range = case splitOnce '-' range of
  Nothing -> Left $ "Failed to parse range: " <> range
  Just (left, right) -> do
    left <- readInt left
    right <- readInt right
    return (left, right)

parseLine :: String -> Either String (Range, Range)
parseLine line = case splitOnce ',' line of
  Nothing -> Left $ "Failed to parse line " <> line
  Just (left, right) -> do
    left <- parseRange left
    right <- parseRange right
    return (left, right)

parse :: String -> Either String [(Range, Range)]
parse input =
  split '\n' input
    <&> trim isSpace
    & filter (not . null)
    & traverse parseLine

rangeContains :: Range -> Range -> Bool
rangeContains (ls, le) (rs, re) = ls <= rs && le >= re

rangesContainEachOther :: Range -> Range -> Bool
rangesContainEachOther a b = rangeContains a b || rangeContains b a

rangesOverlap :: Range -> Range -> Bool
rangesOverlap (ls, le) (rs, re) =
  (rs >= ls && rs <= le)
    || (re >= ls && re <= le)
    || (ls >= rs && ls <= re)
    || (le >= rs && le <= re)

solvePartOne :: [(Range, Range)] -> Int
solvePartOne lines =
  lines
    & filter (uncurry rangesContainEachOther)
    & length

solvePartTwo :: [(Range, Range)] -> Int
solvePartTwo lines =
  lines
    & filter (uncurry rangesOverlap)
    & length

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
