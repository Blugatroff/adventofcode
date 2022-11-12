module Days.Day5 (partOne, partTwo) where

import Data.List
import Text.Read (readEither)
import Util

type Point = (Int, Int)

type Line = (Point, Point)

intoTuple :: [a] -> (a, a)
intoTuple [a, b] = (a, b)
intoTuple b = error "expected 2 elements in list"

parsePoint :: String -> Either String Point
parsePoint input = fmap intoTuple $ traverse readEither $ split ',' input

parseLine :: String -> Either String Line
parseLine input = fmap intoTuple $ traverse parsePoint $ splitSeq " -> " input

parse :: String -> Either String [Line]
parse = traverse parseLine . split '\n'

genLine :: Line -> [Point]
genLine ((x1, y1), (x2, y2)) | x1 == x2 && y1 == y2 = [(x1, y1)]
genLine ((x1, y1), (x2, y2)) = (x1, y1) : genLine ((x1 + sign (x2 - x1), y1 + sign (y2 - y1)), (x2, y2))

isDiagonal :: Line -> Bool
isDiagonal ((x1, y1), (x2, y2)) = x1 /= x2 && y1 /= y2

computeScore :: [Point] -> Int
computeScore points = length $ filter (\(_, c) -> c >= 2) $ dedup points

solvePartOne :: [Line] -> Int
solvePartOne = computeScore . concatMap genLine . filter (not . isDiagonal)

solvePartTwo :: [Line] -> Int
solvePartTwo = computeScore . concatMap genLine

partOne :: String -> Either String String
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input
