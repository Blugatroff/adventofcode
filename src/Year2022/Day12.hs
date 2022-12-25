module Year2022.Day12 (partOne, partTwo) where

import Data.Char (isAsciiLower, isSpace)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Dijkstra
  ( Cell (..),
    Solution (cost),
    World (lookupCell, adjacentCells),
    findSolutionFrom,
  )
import Util (safeHead, safeMinimum, split, trim)

data HeightMapCell = Start | End | Height !Int
  deriving (Eq, Show)

newtype HeightMap = HeightMap (M.Map (Int, Int) HeightMapCell)

instance World HeightMap (Int, Int) where
  lookupCell (x, y) (HeightMap map) = M.lookup (x, y) map <&> toDijkstraCell
    where
      toDijkstraCell :: HeightMapCell -> Cell
      toDijkstraCell Start = Cell 1
      toDijkstraCell End = Destination 1
      toDijkstraCell (Height h) = Cell 1

  adjacentCells (x, y) (HeightMap world) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
      & filter (movePossible)
    where
      movePossible to = case (M.lookup (x, y) world, M.lookup to world) of
        (Just previous, Just this) -> cellHeight previous + 1 >= cellHeight this
        (Nothing, Just this) -> True
        _ -> False


parseCell :: Char -> Either String HeightMapCell
parseCell 'S' = Right Start
parseCell 'E' = Right End
parseCell c | isAsciiLower c = Right $ Height $ fromEnum c - fromEnum 'a'
parseCell c = Left $ "failed to parse cell " <> [c]

parseLine :: String -> Either String [HeightMapCell]
parseLine = traverse parseCell

parse :: String -> Either String HeightMap
parse input = lines <&> assocs <&> M.fromList <&> HeightMap
  where
    assocs :: [[HeightMapCell]] -> [((Int, Int), HeightMapCell)]
    assocs lines = zip [0 ..] lines >>= \(y, l) -> zip [0 ..] l <&> \(x, c) -> ((x, y), c)
    lines = split '\n' input <&> trim isSpace & traverse parseLine

findHeightMapStart :: HeightMap -> Maybe (Int, Int)
findHeightMapStart (HeightMap map) = M.assocs map & filter ((== Start) . snd) & safeHead <&> fst

maximumHeight :: Int
maximumHeight = fromEnum 'z' - fromEnum 'a'

cellHeight :: HeightMapCell -> Int
cellHeight (Height h) = h
cellHeight End = maximumHeight
cellHeight Start = 0

solvePartOne :: HeightMap -> Maybe Int
solvePartOne heightMap = do
  (x, y) <- findHeightMapStart heightMap
  solution <- findSolutionFrom heightMap (x, y)
  return $ cost solution - 1

solvePartTwo :: HeightMap -> Maybe Int
solvePartTwo heightMap@(HeightMap hm) = safeMinimum (cost <$> solutions) <&> subtract 1
  where
    solutions = startingPositions <&> findSolutionFrom heightMap & catMaybes
    startingPositions = M.assocs hm & filter ((== 0) . cellHeight . snd) <&> fst

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
