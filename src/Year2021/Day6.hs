module Year2021.Day6 (partOne, partTwo) where

import MeLude
import qualified Data.Map as M
import Util

parse :: String -> Either String [Int]
parse = traverse readEither . split ','

iterateMap :: v -> Int -> M.Map Int v -> [(Int, v)]
iterateMap def 0 map = []
iterateMap def n map = (n, fromMaybe def $ M.lookup n map) : iterateMap def (n - 1) map

age :: M.Map Int Int -> M.Map Int Int
age = M.fromList . map f . iterateMap 0 8
  where
    f :: (Int, Int) -> (Int, Int)
    f (0, v) = error "cannot age fish in state 0"
    f (k, v) = (k - 1, v)

step :: M.Map Int Int -> M.Map Int Int
step fish = M.insertWith (+) 6 f0 $ M.insertWith (+) 8 f0 $ age $ M.insert 0 0 fish
  where
    f0 :: Int
    f0 = fromMaybe 0 $ M.lookup 0 fish

solvePartTwo :: Int -> [Int] -> Int
solvePartTwo days fish = M.foldl (+) 0 $ (!! days) $ iterate step $ M.fromList $ dedup fish

partOne :: String -> Either String String
partOne input = show . solvePartTwo 80 <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo 256 <$> parse input
