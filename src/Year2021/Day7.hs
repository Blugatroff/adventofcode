module Year2021.Day7 (partOne, partTwo) where

import Text.Read (readEither)
import Util

parse :: String -> Either String [Int]
parse input = traverse readEither $ split ',' input

divRound :: Int -> Int -> Int
divRound a b = if m >= h then (a `div` b) + 1 else a `div` b
  where
    m = a `mod` b
    h = b `div` 2

average :: [Int] -> Int
average list = (`divRound` length list) $ sum list

distance :: Int -> Int -> Int
distance a = abs . (-) a

moveCost :: Int -> Int -> Int
moveCost a b = sum [0 .. distance a b]

fuelCostSum :: (Int -> Int -> Int) -> Int -> [Int] -> Int
fuelCostSum costFn pos = sum . map (costFn pos)

costs :: (Int -> Int -> Int) -> [Int] -> [Int]
costs costFn list = map (flip (fuelCostSum costFn) list) [0 ..]

lastBeforeUp :: [Int] -> Int
lastBeforeUp (a : b : l) | a >= b = lastBeforeUp (b : l)
lastBeforeUp (a : b : l) = a
lastBeforeUp [a] = a
lastBeforeUp [] = error "lastBeforeUp of []"

solvePartOne :: [Int] -> Int
solvePartOne = lastBeforeUp . costs distance

solvePartTwo :: [Int] -> Int
solvePartTwo = lastBeforeUp . costs moveCost

partOne :: String -> Either String String
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input
