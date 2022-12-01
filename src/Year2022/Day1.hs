module Year2022.Day1 (partOne, partTwo) where

import Data.Char (isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Text.Read (readEither, readMaybe)
import Util (readInt, split, trim)
import qualified Util

parseLine :: [Char] -> Either String (Maybe Int)
parseLine [] = Right Nothing
parseLine s = Just <$> readInt s

parse :: String -> Either String [Maybe Int]
parse input = split '\n' input <&> trim isSpace & traverse parseLine

solvePartOne :: [Maybe Int] -> Int
solvePartOne lines = split Nothing lines <&> (<&> fromMaybe 0) <&> sum & maximum

solvePartTwo :: [Maybe Int] -> Int
solvePartTwo lines = split Nothing lines <&> (<&> fromMaybe 0) <&> sum & sort & reverse & take 3 & sum

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
