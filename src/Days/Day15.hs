module Days.Day15 (partOne, partTwo) where

import Control.Monad.State (State)
import Data.Array ((!))
import qualified Data.Array as A
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Heap as Heap
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import GHC.IO (unsafePerformIO)
import Text.Read (readEither)
import Util (split, third, trim)

data Cave = Cave
  { cells :: !(A.Array Int Int),
    width :: !Int,
    height :: !Int
  }

instance Show Cave where
  show cave =
    concatMap (<> "\n") $
      [0 .. height cave - 1] <&> \y ->
        [0 .. width cave - 1] >>= \x -> getCell x y cave & show

isInCave :: Int -> Int -> Cave -> Bool
isInCave x y cave = x >= 0 && y >= 0 && x < width cave && y < height cave

getCell :: Int -> Int -> Cave -> Int
getCell x y cave = cells cave ! (y * width cave + x)

parse :: String -> Either String Cave
parse input = do
  let lines = split '\n' input <&> trim isSpace & filter (not . null)
  rows <- traverse (traverse (readEither . (: []))) lines
  let cells = concat rows
  let width = length $ head rows
  let height = length rows
  let array = A.array (0, length cells - 1) (zip [0 ..] cells)
  Right $ Cave array width height

data Path = PathEnd !(Maybe Path) !Int !Int !Int | PathBranch !(Maybe Path) !Int !Int !Int [Path]

instance Show Path where
  show (PathEnd parent x y cost) = "(PathEnd " <> show x <> " " <> show y <> " " <> show cost <> ")"
  show (PathBranch parent x y cost next) = "(PathBranch " <> show x <> " " <> show y <> " " <> show cost <> ")"

instance Eq Path where
  (PathBranch previous1 x1 y1 cost1 next1) == (PathBranch previous2 x2 y2 cost2 next2) = x1 == x2 && y1 == y2 && cost1 == cost2 && previous1 == previous2
  (PathEnd previous1 x1 y1 cost1) == (PathEnd previous2 x2 y2 cost2) = x1 == x2 && y1 == y2 && cost1 == cost2 && previous1 == previous2
  _ == _ = False

instance Ord Path where
  compare (PathEnd p1 x1 y1 cost1) (PathEnd p2 x2 y2 cost2) = compare cost1 cost2
  compare (PathEnd p1 x1 y1 cost1) (PathBranch p2 x2 y2 cost2 next) = compare cost1 (cost2 + 1)
  compare (PathBranch p1 x1 y1 cost1 next1) (PathBranch p2 x2 y2 cost2 next) = compare cost1 cost2
  compare (PathBranch p1 x1 y1 cost1 next1) (PathEnd p2 x2 y2 cost2) = compare (cost1 + 1) cost2

pathContains :: (Int, Int) -> Path -> Bool
pathContains (tx, ty) (PathEnd previous x y cost) = (tx == x && ty == y) || maybe False (pathContains (tx, ty)) previous
pathContains (tx, ty) (PathBranch previous x y cost next) = (tx == x && ty == y) || maybe False (pathContains (tx, ty)) previous

findPaths :: Maybe Path -> (Int, Int, Int) -> Cave -> Maybe Path
findPaths previous (x, y, cost) cave | x == width cave - 1 && y == height cave - 1 = Just $ PathEnd previous x y $ cost + getCell x y cave
findPaths previous (x, y, cost) cave | isInCave x y cave = if maybe False (pathContains (x, y)) previous then Nothing else Just this
  where
    this = PathBranch previous x y (cost + thisCost) next
    next :: [Path]
    next = catMaybes [left, right, up, down]
      where
        left = findPaths (Just this) (x - 1, y, cost + thisCost) cave
        right = findPaths (Just this) (x + 1, y, cost + thisCost) cave
        up = findPaths (Just this) (x, y - 1, cost + thisCost) cave
        down = findPaths (Just this) (x, y + 1, cost + thisCost) cave
    thisCost = getCell x y cave
findPaths previous (x, y, cost) cave = Nothing

walkPath :: Path -> [[(Int, Int, Int)]]
walkPath (PathEnd previous x y cost) = [[(x, y, cost)]]
walkPath (PathBranch previous x y cost next) = do
  next <&> walkPath & concatMap (<&> ((x, y, cost) :))

type PathQueue = Heap.Heap Path

data Solution = Solution {previous :: !(Maybe Path), x :: !Int, y :: !Int, cost :: !Int}

type AlreadyVisited = Set.Set (Int, Int)

evaluateNextBranch :: (PathQueue, AlreadyVisited) -> Maybe Solution
evaluateNextBranch (queue, visited) = case Heap.viewMin queue of
  Nothing -> Nothing
  Just (PathEnd p x y cost, remainingQueue) -> Just $ Solution p x y cost
  Just (PathBranch p x y cost next, remainingQueue) ->
    evaluateNextBranch $
      if Set.member (x, y) visited
        then (remainingQueue, visited)
        else (foldr Heap.insert remainingQueue next, Set.insert (x, y) visited)

solvePartOne :: Cave -> Int
solvePartOne cave = (findPaths Nothing (0, 0, 0) cave >>= solution & maybe 0 cost) - getCell 0 0 cave
  where
    solution :: Path -> Maybe Solution
    solution root = evaluateNextBranch (Heap.singleton root, Set.empty)

tileCave :: Cave -> Cave
tileCave cave = Cave array (width cave * 5) (height cave * 5)
  where
    array = A.array (0, width cave * height cave * 5 * 5 - 1) (zip [0 ..] cells)
    cells :: [Int]
    cells =
      [0 .. 4] >>= \repeatY ->
        [0 .. height cave - 1] >>= \y ->
          [0 .. 4] >>= \repeatX ->
            [0 .. width cave - 1] <&> \x ->
              (getCell x y cave + repeatX + repeatY - 1) `mod` 9 + 1

solvePartTwo :: Cave -> Int
solvePartTwo = solvePartOne . tileCave

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
