module Year2021.Day3 (partOne, partTwo) where

import Data.List (transpose)
import Util

parseLine :: String -> [Bool]
parseLine = map (== '1')

parse :: String -> [[Bool]]
parse = map parseLine . split '\n'

mostCommon :: [Bool] -> Bool
mostCommon list = case () of
  () | a == b -> True
  () | a < b -> False
  aGreaterThanB -> True
  where
    a = length (filter id list)
    b = length (filter not list)

fromBitsRev :: [Bool] -> Int
fromBitsRev [] = 0
fromBitsRev (True : rest) = 1 + fromBitsRev (False : rest)
fromBitsRev (False : rest) = 2 * fromBitsRev rest

fromBits :: [Bool] -> Int
fromBits = fromBitsRev . reverse

solvePartOne :: [[Bool]] -> Int
solvePartOne lines = fromBits bits * fromBits (map not bits)
  where
    tl = transpose lines
    bits = map mostCommon tl

oxyBitCriteria :: [(Bool, a)] -> [a]
oxyBitCriteria column = map snd $ filter ((== mc) . fst) column
  where
    mc = mostCommon $ map fst column

scrubberBitCriteria :: [(Bool, a)] -> [a]
scrubberBitCriteria column = map snd $ filter ((/= mc) . fst) column
  where
    mc = mostCommon $ map fst column

ratingFromBitCriteria :: ([(Bool, [Bool])] -> [[Bool]]) -> [[Bool]] -> [Bool]
ratingFromBitCriteria criteria = f 0
  where
    f :: Int -> [[Bool]] -> [Bool]
    f cursor [n] = n
    f cursor numbers = f (cursor + 1) $ criteria $ map (\n -> (n !! cursor, n)) numbers

oxyRating :: [[Bool]] -> Int
oxyRating = fromBits . ratingFromBitCriteria oxyBitCriteria

scrubberRating :: [[Bool]] -> Int
scrubberRating = fromBits . ratingFromBitCriteria scrubberBitCriteria

solvePartTwo :: [[Bool]] -> Int
solvePartTwo numbers = oxyRating numbers * scrubberRating numbers

partOne :: String -> Either String String
partOne = Right . show . solvePartOne . parse

partTwo :: String -> Either String String
partTwo = Right . show . solvePartTwo . parse
