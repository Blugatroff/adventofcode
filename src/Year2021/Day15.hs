module Year2021.Day15 (partOne, partTwo) where

import Data.Array ((!))
import Data.Array qualified as A
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import Dijkstra
  ( Cell (..),
    Solution (cost),
    World (adjacentCells, lookupCell, Pos),
    findSolutionFrom,
  )
import Util (readInt, split, trim)
import Data.List (intercalate)
import Data.Pos qualified as P

data Cave = Cave
  { cells :: !(A.Array Int Int),
    width :: !Int,
    height :: !Int
  }

instance World Cave where
  type Pos Cave = P.Pos
  lookupCell (P.Pos x y) cave =
    if isInCave x y cave
      then
        Just $
          (if x == width cave - 1 && y == height cave - 1 then Destination else Cell) $
            getCell x y cave
      else Nothing

  adjacentCells (P.Pos x y) world = [P.Pos (x - 1) y, P.Pos (x + 1) y, P.Pos x (y - 1), P.Pos x (y + 1)]

instance Show Cave where
  show cave = intercalate "\n" $ do
    [0 .. height cave - 1] <&> \y ->
      [0 .. width cave - 1] >>= \x ->
        getCell x y cave & show

isInCave :: Int -> Int -> Cave -> Bool
isInCave x y cave = x >= 0 && y >= 0 && x < width cave && y < height cave

getCell :: Int -> Int -> Cave -> Int
getCell x y cave = cells cave ! (y * width cave + x)

parse :: String -> Either String Cave
parse input = do
  let lines = split '\n' input <&> trim isSpace & filter (not . null)
  rows <- traverse (traverse (readInt . (: []))) lines
  let cells = concat rows
  let width = length $ head rows
  let height = length rows
  let array = A.array (0, length cells - 1) (zip [0 ..] cells)
  Right $ Cave array width height

solvePartOne :: Cave -> Maybe Int
solvePartOne cave = subtract (getCell 0 0 cave) . cost <$> findSolutionFrom cave (P.Pos 0 0)

tileCave :: Cave -> Cave
tileCave cave = Cave array (width cave * 5) (height cave * 5)
  where
    array = A.array (0, width cave * height cave * 5 * 5 - 1) (zip [0 ..] cells)
    cells :: [Int]
    cells =
      [0 .. 4] >>= \repeatY ->
        [0 .. height cave - 1] >>= \y ->
          [0 .. 4] >>= \repeatX ->
            [0 .. width cave - 1] <&> \x ->
              (getCell x y cave + repeatX + repeatY - 1) `mod` 9 + 1

solvePartTwo :: Cave -> Maybe Int
solvePartTwo = solvePartOne . tileCave

partOne :: String -> Either String String
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input
