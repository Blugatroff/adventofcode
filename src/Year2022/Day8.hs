module Year2022.Day8 (partOne, partTwo) where

import MeLude
import qualified Data.Map as M
import Util (readInt, split, trim)

type Grid = M.Map (Int, Int) Int

parse :: String -> Either String Grid
parse input = do
  let lines = trim isSpace <$> split '\n' input
  rows :: [[Int]] <- traverse (traverse readDigit) $ filter (not . null) lines

  pure $ fold $ do
    (y, row) <- zip [0..] rows
    (x, v) <- zip [0..] row
    pure $ M.singleton (x, y) v
  where
    readDigit :: Char -> Either String Int
    readDigit c = readInt [c]


pathsToBorder :: (Int, Int) -> (Int, Int) -> [[(Int, Int)]]
pathsToBorder (width, height) (x, y) = [top, bottom, left, right]
  where
    top :: [(Int, Int)]
    top = reverse [0 .. y - 1] <&> (x,)

    bottom :: [(Int, Int)]
    bottom = [y + 1 .. height - 1] <&> (x,)

    left :: [(Int, Int)]
    left = reverse [0 .. x - 1] <&> (,y)

    right :: [(Int, Int)]
    right = [x + 1 .. width - 1] <&> (,y)

isVisible :: (Int, Int) -> Grid -> (Int, Int) -> Bool
isVisible (width, height) grid (x, y) = pathsToBorder (width, height) (x, y) & any (all isLower)
  where
    h = fromMaybe 0 $ M.lookup (x, y) grid

    isLower :: (Int, Int) -> Bool
    isLower (x, y) = M.lookup (x, y) grid & fromMaybe 0 & (< h)

scenicScore :: (Int, Int) -> Grid -> (Int, Int) -> Int
scenicScore (width, height) grid (x, y) = pathsToBorder (width, height) (x, y) <&> viewDistance & product
  where
    h = fromMaybe 0 $ M.lookup (x, y) grid

    isLower :: (Int, Int) -> Bool
    isLower (x, y) = M.lookup (x, y) grid & fromMaybe 0 & (< h)

    viewDistance :: [(Int, Int)] -> Int
    viewDistance list = if numberSmallerTrees == length list then numberSmallerTrees else numberSmallerTrees + 1
      where
        numberSmallerTrees = length $ takeWhile isLower list

gridWidth :: (Ord k, Num k) => M.Map (k, k) a -> k
gridWidth grid = M.keys grid <&> fst & maximum & (+ 1)

gridHeight :: (Ord k, Num k) => M.Map (k, k) a -> k
gridHeight grid = M.keys grid <&> snd & maximum & (+ 1)

solvePartOne :: Grid -> Int
solvePartOne grid = M.keys grid & filter (isVisible (gridWidth grid, gridHeight grid) grid) & length

solvePartTwo :: Grid -> Int
solvePartTwo grid = M.keys grid <&> scenicScore (gridWidth grid, gridHeight grid) grid & maximum

partOne :: String -> Either String String
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input
