module Year2024.Day6 (partOne, partTwo) where

import Control.Monad (guard)
import Data.Array.IArray (Array, Ix (..), array, assocs, bounds, indices, (!), (//))
import Data.Either.Extra (maybeToEither)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Direction (Direction (..), directionX, directionY, turnRight)
import Util

data Tile = Empty | Obstacle | Guard Direction deriving (Eq, Show)

type LabMap = Array (Int, Int) Tile

parse :: String -> Either String LabMap
parse input = do
  tiles <- traverse (traverse parseTile) $ filter (not . null) $ map trimSpace $ lines input
  let height = length tiles
  let width = length (head tiles)
  Right $ array ((1, 1), (width, height)) $ do
    (y, row) <- zip [1 ..] tiles
    (x, tile) <- zip [1 ..] row
    pure ((x, y), tile)
 where
  parseTile = \case
    '.' -> Right Empty
    '#' -> Right Obstacle
    '^' -> Right $ Guard DirUp
    '>' -> Right $ Guard DirRight
    '<' -> Right $ Guard DirLeft
    'v' -> Right $ Guard DirDown
    c -> Left $ "Failed to parse tile: " <> show c

findGuard :: LabMap -> Either String ((Int, Int), Direction)
findGuard lab = case guards of
  [guard] -> Right guard
  _ -> Left "failed to find exactly one guard"
 where
  guards = mapMaybe (\(pos, tile) -> case tile of Guard dir -> Just (pos, dir); _ -> Nothing) $ assocs lab

walk :: Set ((Int, Int), Direction) -> LabMap -> Direction -> (Int, Int) -> Maybe [(Int, Int)]
walk visited lab dir pos@(x, y) = do
  let pos' = (x + directionX dir, y + directionY dir)
  if inRange (bounds lab) pos'
    then
      if lab ! pos' == Obstacle
        then walk visited lab (turnRight dir) pos
        else do
          if Set.member (pos', dir) visited
            then Nothing
            else (pos' :) <$> walk (Set.insert (pos', dir) visited) lab dir pos'
    else Just []

partOne :: String -> Either String String
partOne input = do
  lab <- parse input
  (pos, dir) <- findGuard lab
  path <- maybeToEither "Cycle detected" $ walk Set.empty lab dir pos
  Right $ show $ length $ dedup $ pos : path

partTwo :: String -> Either String String
partTwo input = do
  lab <- parse input
  (guardPos, dir) <- findGuard lab
  Right $ show $ length $ do
    pos <- indices lab
    guard (pos /= guardPos)
    guard (lab ! pos /= Obstacle)
    let modifiedLab = lab // [(pos, Obstacle)]
    case walk Set.empty modifiedLab dir guardPos of
      Nothing -> [pos]
      Just _ -> []
