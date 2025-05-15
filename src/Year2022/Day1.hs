module Year2022.Day1 (partOne, partTwo) where

import MeLude
import Util (readInt, split, trim)
import Data.Ord (comparing, Down (Down))

parseLine :: String -> Either String (Maybe Int)
parseLine [] = Right Nothing
parseLine s = Just <$> readInt s

parse :: String -> Either String [Maybe Int]
parse input = split '\n' input & traverse (parseLine . trim isSpace)

solvePartOne :: [Maybe Int] -> Int
solvePartOne lines = split Nothing lines <&> ((<&> fromMaybe 0) >>> sum) & maximum

solvePartTwo :: [Maybe Int] -> Int
solvePartTwo lines = split Nothing lines <&> ((<&> fromMaybe 0) >>> sum) & sortBy (comparing Down) & take 3 & sum

partOne :: String -> Either String String
partOne input = parse input <&> (solvePartOne >>> show)

partTwo :: String -> Either String String
partTwo input = parse input <&> (solvePartTwo >>> show)
