module Year2024.Day2 (partOne, partTwo) where
import Util

parse :: String -> Either String [[Int]]
parse =
  traverse (traverse (readInt . trimSpace) . split ' ')
    . filter (not . null)
    . map trimSpace
    . lines

isSafe :: [Int] -> Bool
isSafe levels = do
  let diffSmallEnough (a, b) =
        let diff = abs (a - b)
         in 1 <= diff && diff <= 3
  any (\f -> all (uncurry f) (pairs levels)) [(<), (>)]
    && all diffSmallEnough (pairs levels)

isSafeRegardless :: [Int] -> Bool
isSafeRegardless levels = any isSafe $ levels : holes levels
 where
  holes [] = []
  holes (x : xs) = xs : ((x :) <$> holes xs)

partOne :: String -> Either String String
partOne input = do
  inputs <- parse input
  Right $ show $ length $ filter isSafe inputs

partTwo :: String -> Either String String
partTwo input = show . length . filter isSafeRegardless <$> parse input
