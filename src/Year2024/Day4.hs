module Year2024.Day4 (partOne, partTwo) where

import MeLude
import Data.Pos
import Util

parse :: String -> UArray Pos Char
parse input = do
  let rows = filter (not . null) $ map trimSpace $ lines input
  let h = length rows
  let w = fromMaybe 0 $ safeMaximum $ map length rows
  array (Pos 1 1, Pos w h) $ do
    (y, row) <- zip [1 ..] rows
    (x, c) <- zip [1 ..] row
    pure (Pos x y, c)

rows :: (IArray arr a) => arr Pos a -> [[a]]
rows arr = do
  let (min, max) = bounds arr
  [min.y .. max.y] <&> \y -> [min.x .. max.x] <&> \x -> arr ! Pos x y

columns :: (IArray arr a) => arr Pos a -> [[a]]
columns arr = do
  let (min, max) = bounds arr
  [min.x .. max.x] <&> \x -> [min.y .. max.y] <&> \y -> arr ! Pos x y

width :: (IArray arr a) => arr Pos a -> Int
width arr = let (min, max) = bounds arr in max.x - min.x + 1

height :: (IArray arr a) => arr Pos a -> Int
height arr = let (min, max) = bounds arr in max.y - min.y + 1

diagonals :: (IArray arr a) => arr Pos a -> [[a]]
diagonals arr =
  fold
    [ map (arr !) . diagonalFrom (Pos 1 1) <$> (((`Pos` 1) <$> [1 .. width arr]) <> (Pos 1 <$> [2 .. height arr]))
    , map (arr !) . diagonalFrom (Pos (-1) 1) <$> (((`Pos` 1) <$> [1 .. width arr]) <> (Pos (width arr) <$> [2 .. height arr]))
    ]
 where
  diagonalFrom d p
    | inRange (bounds arr) p = p : diagonalFrom d (d + p)
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

squares :: (IArray arr a) => arr Pos a -> [arr Pos a]
squares arr = do
  topLeft@(Pos tlx tly) <- range $ bounds arr
  let squareBounds = (Pos 1 1, Pos 3 3)
  squareEntries <- for (range squareBounds) $ \(Pos x y) -> do
    let p = topLeft + Pos (x - 1) (y - 1)
    guard (inRange (bounds arr) p)
    pure (Pos x y, arr ! p)
  pure $ array squareBounds squareEntries

squareIsX :: (IArray arr Char) => arr Pos Char -> Bool
squareIsX square = all isMas [diagonal1, diagonal2]
 where
  isMas s = s == "MAS" || s == "SAM"
  diagonal1 = (square !) <$> [Pos 1 1, Pos 2 2, Pos 3 3]
  diagonal2 = (square !) <$> [Pos 3 1, Pos 2 2, Pos 1 3]

partTwo :: String -> Either e String
partTwo = Right . show . length . filter squareIsX . squares . parse
