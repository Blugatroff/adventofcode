module Year2022.Day18 (partOne, partTwo) where

import MeLude
import Data.Map qualified as M
import Data.Set qualified as S
import Util (dedup, readInt, split, trim)

parseLine :: String -> Either String (Int, Int, Int)
parseLine s =
  split ',' s & traverse readInt >>= \case
    [x, y, z] -> Right (x, y, z)
    _ -> Left $ "Expected 3 Integers, got this: " <> s

parse :: String -> Either String [(Int, Int, Int)]
parse input =
  split '\n' input
    <&> trim isSpace
    & filter (not . null)
    & traverse parseLine

neighbours :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbours (x, y, z) =
  [ (x + 1, y, z),
    (x - 1, y, z),
    (x, y + 1, z),
    (x, y - 1, z),
    (x, y, z + 1),
    (x, y, z - 1)
  ]

solvePartOne :: S.Set (Int, Int, Int) -> Int
solvePartOne cubes =
  S.elems cubes
    >>= neighbours
    & filter (not . flip S.member cubes)
    & length

fill :: (Int, Int, Int) -> S.Set (Int, Int, Int) -> Bool
fill from cubes = isNothing $ flow S.empty from
  where
    cubeList = S.elems cubes
    xs = cubeList <&> \(x, _, _) -> x
    ys = cubeList <&> \(_, y, _) -> y
    zs = cubeList <&> \(_, _, z) -> z

    xMin = minimum xs
    xMax = maximum xs
    yMin = minimum ys
    yMax = maximum ys
    zMin = minimum zs
    zMax = maximum zs

    flow :: S.Set (Int, Int, Int) -> (Int, Int, Int) -> Maybe (S.Set (Int, Int, Int))
    flow visited (x, y, z) | x > xMax || x < xMin = Nothing
    flow visited (x, y, z) | y > yMax || y < yMin = Nothing
    flow visited (x, y, z) | z > zMax || z < zMin = Nothing
    flow visited pos | S.member pos visited = Just visited
    flow visited pos | S.member pos cubes = Just visited
    flow visited pos = foldM flow (S.insert pos visited) $ neighbours pos

solvePartTwo :: S.Set (Int, Int, Int) -> Int
solvePartTwo cubes = length $ filter (fromMaybe False . flip M.lookup lookup) neis
  where
    neis = S.elems cubes >>= neighbours
    distinctNeighbours = fst <$> dedup neis
    lookup = distinctNeighbours <&> (\pos -> (pos, fill pos cubes)) & M.fromList

partOne :: String -> Either String String
partOne input = show . solvePartOne . S.fromList <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo . S.fromList <$> parse input
