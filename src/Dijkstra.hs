module Dijkstra
  ( World (..),
    Cell (..),
    Solution (..),
    findSolutionFrom,
    cellCost,
  )
where

import MeLude
import Data.Heap qualified as Heap
import Data.Kind (Type)
import Data.Set qualified as Set

class (Ord (Pos world)) => World world where
  type Pos world :: Type
  lookupCell :: Pos world -> world -> Maybe Cell
  adjacentCells :: Pos world -> world -> [Pos world]

data Solution pos = Solution {path :: [pos], cost :: Int}
  deriving (Show)

data Cell = Destination !Int | Cell !Int
  deriving (Show)

cellCost :: Cell -> Int
cellCost (Destination cost) = cost
cellCost (Cell cost) = cost

data Path pos = PathEnd !(Maybe (Path pos)) !pos !Int !Int | PathBranch !(Maybe (Path pos)) !pos !Int [Path pos]

instance Show pos => Show (Path pos) where
  show (PathEnd _ pos cost cellCost) = "(PathEnd " <> show pos <> " " <> show cost <> " " <> show cellCost <> ")"
  show (PathBranch _ pos cost _) = "(PathBranch " <> show pos <> " " <> show cost <> ")"

instance Eq pos => Eq (Path pos) where
  (PathBranch parent1 pos1 cost1 _) == (PathBranch parent2 pos2 cost2 _) = pos1 == pos2 && cost1 == cost2 && parent1 == parent2
  (PathEnd parent1 pos1 cost1 cellCost1) == (PathEnd parent2 pos2 cost2 cellCost2) = pos1 == pos2 && cost1 == cost2 && cellCost1 == cellCost2 && parent1 == parent2
  _ == _ = False

instance Eq pos => Ord (Path pos) where
  compare (PathEnd _ _ cost1 _) (PathEnd _ _ cost2 _) = compare cost1 cost2
  compare (PathBranch _ _ cost1 _) (PathBranch _ _ cost2 _) = compare cost1 cost2
  compare (PathBranch _ _ cost1 _) (PathEnd _ _ cost2 _) = compare cost1 cost2
  compare (PathEnd _ _ cost1 _) (PathBranch _ _ cost2 _) = compare cost1 cost2

findPaths :: World world => Maybe (Path (Pos world)) -> (Pos world, Int) -> world -> Maybe (Path (Pos world))
findPaths previous (pos, cost) world =
  lookupCell pos world <&> \cell ->
    let thisCost = cellCost cell
        next =
          mapMaybe (\p -> findPaths (Just this) (p, cost + thisCost) world) (adjacentCells pos world)
        this = case cell of
          Destination _ -> PathEnd previous pos cost thisCost
          _ -> PathBranch previous pos cost next
     in this

type PathQueue pos = Heap.Heap (Path pos)

type Visited pos = Set.Set pos

evaluateNextBranch :: Ord pos => PathQueue pos -> Visited pos -> Maybe (Solution pos)
evaluateNextBranch queue visited = case Heap.viewMin queue of
  Nothing -> Nothing
  Just (PathEnd p pos cost cellCost, _) -> Just $ Solution (reverse $ pos : maybe [] buildPath p) (cost + cellCost)
  Just (PathBranch _ pos _ next, remainingQueue) ->
    if Set.member pos visited
      then evaluateNextBranch remainingQueue visited
      else evaluateNextBranch (foldl' (flip Heap.insert) remainingQueue next) (Set.insert pos visited)

buildPath :: Path pos -> [pos]
buildPath (PathEnd previous pos _ _) = pos : maybe [] buildPath previous
buildPath (PathBranch previous pos _ _) = pos : maybe [] buildPath previous

findSolutionFrom :: World world => world -> Pos world -> Maybe (Solution (Pos world))
findSolutionFrom world pos = do
  root <- findPaths Nothing (pos, 0) world
  evaluateNextBranch (Heap.singleton root) Set.empty
