module Year2022.Day6 (partOne, partTwo) where

import MeLude
import qualified Data.Set as S

windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows size list = take size list : windows size (drop 1 list)

allDifferent :: Ord a => [a] -> Bool
allDifferent list = length list == S.size (S.fromList list)

solve :: Int -> String -> Int
solve l stream =
  windows l stream
    & takeWhile (not . allDifferent)
    & length
    & (+ l)

partOne :: String -> Either String String
partOne input = solve 4 input & show & Right

partTwo :: String -> Either String String
partTwo input = solve 14 input & show & Right
