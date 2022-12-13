module Dijkstra
  ( World (..),
    Cell (..),
    Solution (..),
    findSolutionFrom,
    cellCost,
  )
where

import Data.Maybe (catMaybes)
import qualified Data.Heap as Heap
import qualified Data.Set as Set

class World world where
  lookupCell :: (Int, Int) -> world -> Maybe Cell
  movePossible :: Maybe (Int, Int) -> (Int, Int) -> world -> Bool

data Solution = Solution {path :: [(Int, Int)], cost :: Int}

data Cell = Destination !Int | Cell !Int

cellCost :: Cell -> Int
cellCost (Destination cost) = cost
cellCost (Cell cost) = cost

data Path = PathEnd !(Maybe Path) !Int !Int !Int !Int | PathBranch !(Maybe Path) !Int !Int !Int [Path]

instance Show Path where
  show (PathEnd _ x y cost cellCost) = "(PathEnd " <> show x <> " " <> show y <> " " <> show cost <> " " <> show cellCost <> ")"
  show (PathBranch _ x y cost _) = "(PathBranch " <> show x <> " " <> show y <> " " <> show cost <> ")"

instance Eq Path where
  (PathBranch parent1 x1 y1 cost1 _) == (PathBranch parent2 x2 y2 cost2 _) = x1 == x2 && y1 == y2 && cost1 == cost2 && parent1 == parent2
  (PathEnd parent1 x1 y1 cost1 cellCost1) == (PathEnd parent2 x2 y2 cost2 cellCost2) = x1 == x2 && y1 == y2 && cost1 == cost2 && cellCost1 == cellCost2 && parent1 == parent2
  _ == _ = False

instance Ord Path where
  compare (PathEnd _ _ _ cost1 _) (PathEnd _ _ _ cost2 _) = compare cost1 cost2
  compare (PathBranch _ _ _ cost1 _) (PathBranch _ _ _ cost2 _) = compare cost1 cost2
  compare (PathBranch _ _ _ cost1 _) (PathEnd _ _ _ cost2 _) = compare cost1 cost2
  compare (PathEnd _ _ _ cost1 _) (PathBranch _ _ _ cost2 _) = compare cost1 cost2

pathPosition :: Path -> (Int, Int)
pathPosition (PathBranch _ x y _ _) = (x, y)
pathPosition (PathEnd _ x y _ _) = (x, y)

findPaths :: World world => Maybe Path -> (Int, Int, Int) -> world -> Maybe Path
findPaths previous (x, y, cost) world = case lookupCell (x, y) world of
  Nothing -> Nothing
  Just cell -> this cell
  where
    this cell =
      if movePossible (pathPosition <$> previous) (x, y) world
        then Just this
        else Nothing
      where
        this = case cell of
          Destination _ -> PathEnd previous x y cost thisCost
          _ -> PathBranch previous x y cost next

        thisCost = cellCost cell

        next = catMaybes [left, right, up, down]

        left = findPaths (Just this) (x - 1, y, cost + thisCost) world
        right = findPaths (Just this) (x + 1, y, cost + thisCost) world
        up = findPaths (Just this) (x, y - 1, cost + thisCost) world
        down = findPaths (Just this) (x, y + 1, cost + thisCost) world

type PathQueue = Heap.Heap Path

type Visited = Set.Set (Int, Int)

evaluateNextBranch :: PathQueue -> Visited -> Maybe Solution
evaluateNextBranch queue visited = case Heap.viewMin queue of
  Nothing -> Nothing
  Just (PathEnd p x y cost cellCost, _) -> Just $ Solution (reverse $ (x, y) : maybe [] buildPath p) (cost + cellCost)
  Just (PathBranch _ x y _ next, remainingQueue) ->
    if Set.member (x, y) visited
      then evaluateNextBranch remainingQueue visited
      else evaluateNextBranch (foldr Heap.insert remainingQueue next) (Set.insert (x, y) visited)

buildPath :: Path -> [(Int, Int)]
buildPath (PathEnd previous x y _ _) = (x, y) : maybe [] buildPath previous
buildPath (PathBranch previous x y _ _) = (x, y) : maybe [] buildPath previous

findSolutionFrom :: World world => world -> (Int, Int) -> Maybe Solution
findSolutionFrom world (x, y) = do
  root <- findPaths Nothing (x, y, 0) world
  evaluateNextBranch (Heap.singleton root) Set.empty
