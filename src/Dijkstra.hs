module Dijkstra
  ( World (..),
    Cell (..),
    Solution (..),
    findSolutionFrom,
    cellCost,
  )
where

import Data.Function (on, (&))
import qualified Data.Heap as Heap
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import qualified Data.Set as Set
import Util (trace)

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
  show (PathEnd parent x y cost cellCost) = "(PathEnd " <> show x <> " " <> show y <> " " <> show cost <> " " <> show cellCost <> ")"
  show (PathBranch parent x y cost next) = "(PathBranch " <> show x <> " " <> show y <> " " <> show cost <> ")"

instance Eq Path where
  (PathBranch parent1 x1 y1 cost1 next1) == (PathBranch parent2 x2 y2 cost2 next2) = x1 == x2 && y1 == y2 && cost1 == cost2 && parent1 == parent2
  (PathEnd parent1 x1 y1 cost1 cellCost1) == (PathEnd parent2 x2 y2 cost2 cellCost2) = x1 == x2 && y1 == y2 && cost1 == cost2 && cellCost1 == cellCost2 && parent1 == parent2
  _ == _ = False

instance Ord Path where
  compare (PathEnd parent1 x1 y1 cost1 cellCost1) (PathEnd parent2 x2 y2 cost2 cellCost2) = compare cost1 cost2
  compare (PathBranch parent1 x1 y1 cost1 next1) (PathBranch parent2 x2 y2 cost2 next2) = compare cost1 cost2
  compare (PathBranch parent1 x1 y1 cost1 next1) (PathEnd parent2 x2 y2 cost2 cellCost2) = compare cost1 cost2
  compare (PathEnd parent1 x1 y1 cost1 cellCost1) (PathBranch parent2 x2 y2 cost2 next2) = compare cost1 cost2

pathPrevious :: Path -> Maybe Path
pathPrevious (PathEnd previous _ _ _ _) = previous
pathPrevious (PathBranch previous _ _ _ _) = previous

pathPosition :: Path -> (Int, Int)
pathPosition (PathBranch _ x y _ _) = (x, y)
pathPosition (PathEnd _ x y _ _) = (x, y)

findPaths :: World world => Maybe Path -> (Int, Int, Int) -> world -> Maybe Path
findPaths previous (x, y, cost) world = case lookupCell (x, y) world of
  Nothing -> Nothing
  Just cell -> this previousHeight cell
  where
    previousHeight :: Int
    previousHeight = case previous of
      Nothing -> 0
      Just previous -> lookupCell (pathPosition previous) world & maybe 0 cellCost

    this previousHeight cell =
      if movePossible (pathPosition <$> previous) (x, y) world
        then Just this
        else Nothing
      where
        this = case cell of
          Destination _ -> PathEnd previous x y cost thisCost
          notEndCell -> PathBranch previous x y cost next

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
  Just (PathEnd p x y cost cellCost, remainingQueue) -> Just $ Solution (reverse $ (x, y) : maybe [] buildPath p) (cost + cellCost)
  Just (PathBranch p x y cost next, remainingQueue) ->
    if Set.member (x, y) visited
      then evaluateNextBranch remainingQueue visited
      else evaluateNextBranch (foldr Heap.insert remainingQueue next) (Set.insert (x, y) visited)

buildPath :: Path -> [(Int, Int)]
buildPath (PathEnd previous x y cost cellCost) = (x, y) : maybe [] buildPath previous
buildPath (PathBranch previous x y cost _) = (x, y) : maybe [] buildPath previous

findSolutionFrom :: World world => world -> (Int, Int) -> Maybe Solution
findSolutionFrom world (x, y) = do
  root <- findPaths Nothing (x, y, 0) world
  evaluateNextBranch (Heap.singleton root) Set.empty
