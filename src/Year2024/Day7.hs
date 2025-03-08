module Year2024.Day7 (partOne, partTwo) where

import MeLude
import Util (parFilter, readInt, split, splitOnce, trimSpace)

type Equation = (Int, [Int])

parse :: String -> Either String [Equation]
parse = traverse parseEquation . filter (not . null) . map trimSpace . lines
 where
  parseEquation eq = do
    let err = "failed to parse equation: " <> eq
    (left, right) <- maybeToEither err $ splitOnce ':' eq
    expected <- readInt left
    nums <- traverse readInt $ filter (not . null) $ map trimSpace $ split ' ' right
    pure (expected, nums)

type Operation = Int -> Int -> Int

couldPossiblyBeTrue :: [Operation] -> Equation -> Bool
couldPossiblyBeTrue operations (expected, []) = False
couldPossiblyBeTrue operations (expected, first : nums) = calculate nums first
 where
  calculate _ v | v > expected = False
  calculate [] v = v == expected
  calculate (x : xs) v = any (\f -> calculate xs $ f v x) operations

solution :: [Operation] -> [Equation] -> Int
solution operations = sum . map fst . parFilter (couldPossiblyBeTrue operations)

partOne :: String -> Either String String
partOne = fmap (show . solution [(*), (+)]) . parse

concatenate :: Operation
concatenate x y = x * (10 ^ ySize) + y
 where
  ySize :: Int = ceiling (logBase 10 (fromIntegral (y + 1)) :: Float)

partTwo :: String -> Either String String
partTwo = fmap (show . solution [(*), (+), concatenate]) . parse
