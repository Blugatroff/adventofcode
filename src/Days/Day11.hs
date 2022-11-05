module Days.Day11 where

import Data.Function ((&))
import Text.Read (readEither)
import Util (mapSnd, split)

type Grid = [[Int]]

readChar :: Read a => Char -> Either String a
readChar char = readEither [char]

parse :: String -> Either String Grid
parse input = grid
  where
    lines = split '\n' input & filter (not . null)
    grid = traverse (traverse readChar) lines

increment :: Grid -> Grid
increment = map (map (+ 1))

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

withCoords :: Grid -> [[(Int, Int, Int)]]
withCoords grid = map forRow (enumerate grid)
  where
    forRow (y, row) = map (\(x, cell) -> (x, y, cell)) (enumerate row)

flash :: Grid -> Int -> Int -> Grid
flash grid tx ty = map (map flashCell) (withCoords grid)
  where
    flashCell :: (Int, Int, Int) -> Int
    flashCell (x, y, cell) | x == tx && y == ty = 0
    flashCell (x, y, cell) | cell == 0 = cell
    flashCell (x, y, cell) | (abs (x - tx) <= 1) && (abs (y - ty) <= 1) = cell + 1
    flashCell (_, _, cell) = cell

findFlashing :: Grid -> [(Int, Int)]
findFlashing grid =
  concat (withCoords grid)
    & filter (\(x, y, cell) -> cell > 9)
    & map (\(x, y, cell) -> (x, y))

step :: Grid -> (Grid, Int)
step grid = f (increment grid)
  where
    f :: Grid -> (Grid, Int)
    f grid = if flashed > 0 then mapSnd (+ flashed) (f newGrid) else (newGrid, flashed)
      where
        (newGrid, flashed) = foldr fold (grid, 0) (findFlashing grid)

    fold :: (Int, Int) -> (Grid, Int) -> (Grid, Int)
    fold (x, y) (grid, flashed) = (flash grid x y, flashed + 1)

solvePartOne :: Grid -> Int
solvePartOne grid = snd $ foldr fold (grid, 0) [1 .. 100]
  where
    fold _ (grid, flashes) = step grid & mapSnd (+ flashes)

solvePartTwo :: Grid -> Int
solvePartTwo = f 1
  where
    f :: Int -> Grid -> Int
    f n grid = if flashes == length grid * length (head grid) then n else f (n + 1) newGrid
      where
        (newGrid, flashes) = step grid

partOne :: String -> Either String String
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input
