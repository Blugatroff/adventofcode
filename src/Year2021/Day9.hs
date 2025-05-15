module Year2021.Day9 (partOne, partTwo) where

import MeLude
import qualified Data.Map as M
import Util (split)
import Data.Ord (comparing, Down (Down))

parse :: String -> Either String [[Int]]
parse input =
  split '\n' input
    & filter (not . null)
    & traverse (traverse (\c -> readEither [c]))

isLowPoint :: [[Int]] -> Int -> Int -> Bool
isLowPoint grid x y =
  (y == 0 || (grid !! (y - 1) !! x) > h)
    && (x == 0 || (grid !! y !! (x - 1)) > h)
    && (y + 1 >= length grid || (grid !! (y + 1) !! x) > h)
    && (x + 1 >= length (grid !! y) || (grid !! y !! (x + 1)) > h)
  where
    h = grid !! y !! x

solvePartOne :: [[Int]] -> Int
solvePartOne grid =
  zipWith f grid [0 ..]
    & concat
    & filter (\(y, x, _) -> isLowPoint grid x y)
    & map (\(y, x, h) -> h + 1)
    & sum
  where
    f col y = zipWith (\h x -> (y, x, h)) col [0 ..]

flow :: [[Int]] -> Int -> Int -> (Int, Int)
flow grid x y | isLowPoint grid x y = (x, y)
flow grid x y | x > 0 && grid !! y !! (x - 1) < (grid !! y !! x) = flow grid (x - 1) y
flow grid x y | y > 0 && grid !! (y - 1) !! x < (grid !! y !! x) = flow grid x (y - 1)
flow grid x y | x + 1 < length (grid !! y) && (grid !! y !! (x + 1) < (grid !! y !! x)) = flow grid (x + 1) y
flow grid x y | y + 1 < length grid && (grid !! (y + 1) !! x < (grid !! y !! x)) = flow grid x (y + 1)
flow grid x y = error "nowhere to flow"

solvePartTwo :: [[Int]] -> Int
solvePartTwo grid =
  foldl'
    f
    M.empty
    ( blocks
        & filter (\(_, _, h) -> h /= 9)
        & map (\(x, y, h) -> flow grid x y)
    )
    & M.toList
    & map snd
    & sortBy (comparing Down)
    & take 3
    & product
  where
    f :: M.Map (Int, Int) Int -> (Int, Int) -> M.Map (Int, Int) Int
    f m (y, x) = M.alter (\c -> Just $ fromMaybe 0 c + 1) (x, y) m

    blocks :: [(Int, Int, Int)]
    blocks =
      zipWith (\col y -> zipWith (\h x -> (x, y, h)) col [0 ..]) grid [0 ..]
        & concat

partOne :: String -> Either String String
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input
