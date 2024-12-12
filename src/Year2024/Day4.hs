module Year2024.Day4 (partOne, partTwo) where

import Control.Monad (guard)
import Data.Array.IArray (IArray, Ix (..), array, bounds, (!))
import Data.Array.Unboxed (UArray)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Util

parse :: String -> UArray (Int, Int) Char
parse input = do
  let rows = filter (not . null) $ map trimSpace $ lines input
  let h = length rows
  let w = fromMaybe 0 $ safeMaximum $ map length rows
  array ((1, 1), (w, h)) $ do
    (y, row) <- zip [1 ..] rows
    (x, c) <- zip [1 ..] row
    pure ((x, y), c)

rows :: (IArray arr a) => arr (Int, Int) a -> [[a]]
rows arr = do
  let ((minX, minY), (maxX, maxY)) = bounds arr
  [minY .. maxY] <&> \y -> [minX .. maxX] <&> \x -> arr ! (x, y)

columns :: (IArray arr a) => arr (Int, Int) a -> [[a]]
columns arr = do
  let ((minX, minY), (maxX, maxY)) = bounds arr
  [minX .. maxX] <&> \x -> [minY .. maxY] <&> \y -> arr ! (x, y)

width :: (IArray arr a) => arr (Int, Int) a -> Int
width arr = let ((minX, _), (maxX, _)) = bounds arr in maxX - minX + 1

height :: (IArray arr a) => arr (Int, Int) a -> Int
height arr = let ((minX, _), (maxX, _)) = bounds arr in maxX - minX + 1

diagonals :: (IArray arr a) => arr (Int, Int) a -> [[a]]
diagonals arr =
  fold
    [ map (arr !) . diagonalFrom (1, 1) <$> (((,1) <$> [1 .. width arr]) <> ((1,) <$> [2 .. height arr]))
    , map (arr !) . diagonalFrom (-1, 1) <$> (((,1) <$> [1 .. width arr]) <> ((width arr,) <$> [2 .. height arr]))
    ]
 where
  diagonalFrom (dx, dy) p@(x, y)
    | inRange (bounds arr) p = p : diagonalFrom (dx, dy) (x + dx, y + dy)
  diagonalFrom _ _ = []

countXmas :: String -> Int
countXmas = go 0
 where
  go !acc ('X' : 'M' : 'A' : 'S' : rest) = go (acc + 1) rest
  go acc (x : xs) = go acc xs
  go acc [] = acc

partOne :: String -> Either e String
partOne input = Right $ show $ do
  let puzzle = parse input
  let diags = diagonals puzzle
  let sequences = diags <> rows puzzle <> columns puzzle
  sum $ map countXmas $ sequences <> map reverse sequences

squares :: (IArray arr a) => arr (Int, Int) a -> [arr (Int, Int) a]
squares arr = do
  topLeft@(tlx, tly) <- range $ bounds arr
  let squareBounds = ((1, 1), (3, 3))
  squareEntries <- for (range squareBounds) $ \(x, y) -> do
    let p = (tlx + x - 1, tly + y - 1)
    guard (inRange (bounds arr) p)
    pure ((x, y), arr ! p)
  pure $ array squareBounds squareEntries

squareIsX :: (IArray arr Char) => arr (Int, Int) Char -> Bool
squareIsX square = all isMas [diagonal1, diagonal2]
 where
  isMas s = s == "MAS" || s == "SAM"
  diagonal1 = (square !) <$> [(1, 1), (2, 2), (3, 3)]
  diagonal2 = (square !) <$> [(3, 1), (2, 2), (1, 3)]

partTwo :: String -> Either e String
partTwo = Right . show . length . filter squareIsX . squares . parse
