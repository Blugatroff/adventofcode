module Year2022.Day12 (partOne, partTwo) where

import MeLude
import Data.Map qualified as M
import Data.Pos (Pos (..))
import Dijkstra qualified
import Util (safeHead, safeMinimum, split, trim)

data HeightMapCell = Start | End | Height !Int
  deriving (Eq, Show)

newtype HeightMap = HeightMap (M.Map Pos HeightMapCell)

instance Dijkstra.World HeightMap where
  type Pos HeightMap = Pos
  lookupCell (Pos x y) (HeightMap map) = M.lookup (Pos x y) map <&> toDijkstraCell
    where
      toDijkstraCell :: HeightMapCell -> Dijkstra.Cell
      toDijkstraCell Start = Dijkstra.Cell 1
      toDijkstraCell End = Dijkstra.Destination 1
      toDijkstraCell (Height h) = Dijkstra.Cell 1

  adjacentCells (Pos x y) (HeightMap world) =
    [Pos (x - 1) y, Pos (x + 1) y, Pos x (y - 1), Pos x (y + 1)]
      & filter movePossible
    where
      movePossible to = case (M.lookup (Pos x y) world, M.lookup to world) of
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
parse input = HeightMap . M.fromList . assocs <$> lines
  where
    assocs :: [[HeightMapCell]] -> [(Pos, HeightMapCell)]
    assocs lines = zip [0 ..] lines >>= \(y, l) -> zip [0 ..] l <&> \(x, c) -> (Pos x y, c)
    lines = traverse (parseLine . trim isSpace) (split '\n' input)

findHeightMapStart :: HeightMap -> Maybe Pos
findHeightMapStart (HeightMap map) = M.assocs map & filter ((== Start) . snd) & safeHead <&> fst

maximumHeight :: Int
maximumHeight = fromEnum 'z' - fromEnum 'a'

cellHeight :: HeightMapCell -> Int
cellHeight (Height h) = h
cellHeight End = maximumHeight
cellHeight Start = 0

solvePartOne :: HeightMap -> Maybe Int
solvePartOne heightMap = do
  Pos {x, y} <- findHeightMapStart heightMap
  solution <- Dijkstra.findSolutionFrom heightMap (Pos x y)
  return $ Dijkstra.cost solution - 1

solvePartTwo :: HeightMap -> Maybe Int
solvePartTwo heightMap@(HeightMap hm) = safeMinimum (Dijkstra.cost <$> solutions) <&> subtract 1
  where
    solutions = mapMaybe (Dijkstra.findSolutionFrom heightMap) startingPositions
    startingPositions = M.assocs hm & filter ((== 0) . cellHeight . snd) <&> fst

partOne :: String -> Either String String
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input
