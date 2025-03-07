module Year2024.Day10 (partOne, partTwo) where

import Control.Monad (guard)
import Data.Array.Base (IArray (..), UArray, (!))
import Data.Array.Base qualified as Array
import Data.Ix (Ix (..))
import Data.Pos (Pos (..))
import Data.Set qualified as Set
import Direction (allDirections)
import Util

type HeightMap = UArray Pos Int
type Path = [Pos]

parse :: String -> Either String HeightMap
parse = parseGrid parseDigit

findTrailheads :: HeightMap -> [Pos]
findTrailheads heightMap = map fst $ filter ((== 0) . snd) $ Array.assocs heightMap

findPaths :: HeightMap -> Pos -> [Path]
findPaths heightMap = go [] Set.empty
 where
  go path visited pos = do
    guard $ inRange (bounds heightMap) pos
    let height = heightMap ! pos
    if height == 9
      then pure [pos]
      else do
        dir <- allDirections
        let neighbour = pos + Pos dir.x dir.y
        guard $ inRange (bounds heightMap) neighbour
        let neighbourHeight = heightMap ! neighbour
        guard $ neighbourHeight == height + 1
        guard $ not $ Set.member neighbour visited
        go (pos : path) (Set.insert pos visited) neighbour

partOne :: String -> Either String String
partOne input = do
  heightMap <- parse input
  pure $ show $ sum $ length . deduplicatePathsToSameDestination . findPaths heightMap <$> findTrailheads heightMap
 where
  deduplicatePathsToSameDestination :: [Path] -> [Path]
  deduplicatePathsToSameDestination = map fst . dedupBy head

partTwo :: String -> Either String String
partTwo input = do
  heightMap <- parse input
  pure $ show $ sum $ length . findPaths heightMap <$> findTrailheads heightMap
