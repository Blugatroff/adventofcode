module Year2024.Day9 (partOne, partTwo) where

import MeLude
import Data.Map.Strict qualified as Map
import Data.Pos (Pos (..), divPos)
import Data.Set qualified as Set
import Util (trimSpace, tuplePermutations)

type Bounds = (Pos, Pos)
type Input = (Bounds, Map Pos Char)

parse :: String -> Either String Input
parse input = do
  let ls = filter (not . null) $ map trimSpace $ lines input
  let height = length ls
  let width = case ls of
        [] -> 0
        (x : xs) -> length x
  unless (all ((== width) . length) ls) $ do
    Left "Input is not a rectangle"
  let antennas = Map.fromList $ do
        (y, row) <- zip [1 ..] ls
        (x, tile) <- zip [1 ..] row
        guard (tile /= '.')
        pure (Pos x y, tile)
  Right ((1, Pos width height), antennas)

groupAntennas :: Map Pos Char -> Map Char [Pos]
groupAntennas = Map.fromListWith (<>) . map (second singleton . swap) . filter ((/= '.') . snd) . Map.assocs

solution :: (Bounds -> Pos -> Pos -> [Pos]) -> Input -> Int
solution findAntinodes (bounds, input) = Set.size $ Set.filter (inRange bounds) antinodes
 where
  antinodes :: Set Pos
  antinodes = foldMap addAntinodesOfFrequency $ Map.assocs $ groupAntennas input

  addAntinodesOfFrequency :: (Char, [Pos]) -> Set Pos
  addAntinodesOfFrequency (freq, antennas) = Set.fromList $ concatMap (uncurry $ findAntinodes bounds) $ tuplePermutations antennas

shoot :: Pos -> Pos -> [Pos]
shoot a b = [b + (b - a), a + (a - b)]

ray :: Bounds -> Pos -> Pos -> [Pos]
ray bounds a b = do
  let delta = b - a
      dir = delta `divPos` gcd delta.x delta.y
      takeInBounds = takeWhile (inRange bounds)
  takeInBounds (iterate (+ dir) a) <> takeInBounds (iterate (+ (-dir)) b)

partOne :: String -> Either String String
partOne = fmap (show . solution (const shoot)) . parse

partTwo :: String -> Either String String
partTwo = fmap (show . solution ray) . parse
