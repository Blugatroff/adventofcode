module Year2024.Day8 (partOne, partTwo) where

import Control.Monad (unless)
import Data.Array.Unboxed (Array, array, assocs, bounds)
import Data.Bifunctor (second)
import Data.Ix (Ix (..))
import Data.List (singleton)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Pos
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Util (trimSpace, tuplePermutations)

type Input = Array Pos Char

parse :: String -> Either String Input
parse input = do
  let ls = filter (not . null) $ map trimSpace $ lines input
  let height = length ls
  let width = case ls of
        [] -> 0
        (x : xs) -> length x
  unless (all ((== width) . length) ls) $ do
    Left "Input is not a rectangle"
  Right $ array (Pos 1 1, Pos width height) $ do
    (y, row) <- zip [1 ..] ls
    (x, tile) <- zip [1 ..] row
    pure (Pos x y, tile)

findAntennas :: Input -> [(Pos, Char)]
findAntennas = filter ((/= '.') . snd) . assocs

groupAntennas :: [(Pos, Char)] -> Map Char [Pos]
groupAntennas = Map.fromListWith (<>) . map (second singleton . swap)

solution :: ((Pos, Pos) -> Pos -> Pos -> [Pos]) -> Input -> Int
solution findAntinodes input = Set.size $ Set.filter (inRange (bounds input)) antinodes
 where
  antinodes :: Set Pos
  antinodes = foldMap addAntinodesOfFrequency $ Map.assocs $ groupAntennas $ findAntennas input

  addAntinodesOfFrequency :: (Char, [Pos]) -> Set Pos
  addAntinodesOfFrequency (freq, antennas) = Set.fromList $ concatMap (uncurry $ findAntinodes (bounds input)) $ tuplePermutations antennas

shoot :: Pos -> Pos -> [Pos]
shoot a b = [b + (b - a), a + (a - b)]

ray :: (Pos, Pos) -> Pos -> Pos -> [Pos]
ray bounds a b = do
  let delta = b - a
      dir = delta `divPos` gcd delta.x delta.y
      takeInBounds = takeWhile (inRange bounds)
  takeInBounds (iterate (+ dir) a) <> takeInBounds (iterate (+ (-dir)) b)

partOne :: String -> Either String String
partOne = fmap (show . solution (const shoot)) . parse

partTwo :: String -> Either String String
partTwo = fmap (show . solution ray) . parse
