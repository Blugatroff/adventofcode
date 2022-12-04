{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Year2021.Day12 (partOne, partTwo) where

import Data.Char (isUpper)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Util (remove, set, split, trim)

data Node = Small !String | Big !String
  deriving (Eq)

type Link = (Node, Node)

type Cave = M.Map String [Node]

type Visited = [Node]

type Path = [Node]

data PathTree = Arrived | Branches ![(String, PathTree)]
  deriving (Show)

nodeName :: Node -> String
nodeName (Small name) = name
nodeName (Big name) = name

node :: String -> Node
node str = case any isUpper str of
  True -> Big str
  False -> Small str

parseLine :: String -> Either String Link
parseLine input = case map node $ split '-' input of
  [left, right] -> Right (left, right)
  _nottwosegmns -> Left $ "failed to parse line: " <> input

parse :: String -> Either String [Link]
parse input =
  split '\n' input
    & filter (not . null)
    & mapM parseLine

addLinkToCave :: Link -> Cave -> Cave
addLinkToCave (left, right) = addRightToLeft . addLeftToRight
  where
    leftName = nodeName left
    rightName = nodeName right
    addRightToLeft cave = case M.lookup leftName cave of
      Just links -> M.insert leftName (right : links) cave
      Nothing -> M.insert leftName [right] cave
    addLeftToRight cave = case M.lookup rightName cave of
      Just links -> M.insert rightName (left : links) cave
      Nothing -> M.insert rightName [left] cave

buildCave :: [Link] -> Cave
buildCave = foldr addLinkToCave M.empty

buildPathTree :: Visited -> Node -> Node -> Cave -> Either String PathTree
buildPathTree visited from to cave | from == to = Right Arrived
buildPathTree visited from to cave = do
  neighbours <- filterNeighbours <$> lookup from
  Branches <$> mapM launchNext neighbours
  where
    lookup node = case M.lookup (nodeName node) cave of
      Just node -> Right node
      Nothing -> Left $ "failed to find node " <> nodeName node
    launchNext neighbour = buildPathTree (from : visited) neighbour to cave <&> (nodeName neighbour,)
    filterNeighbours :: [Node] -> [Node]
    filterNeighbours neighbours = filter canBeVisitedAgain neighbours
      where
        canBeVisitedAgain :: Node -> Bool
        canBeVisitedAgain (Big _) = True
        canBeVisitedAgain (Small name) = all (\n -> name /= nodeName n) visited

pathTree :: Cave -> Either String PathTree
pathTree cave = buildPathTree [] (node "start") (node "end") cave <&> \tree -> Branches [("start", tree)]

buildPathTreePartTwo :: Bool -> Visited -> Node -> Node -> Cave -> Either String PathTree
buildPathTreePartTwo hasUsedJoker visited from to cave | from == to = Right Arrived
buildPathTreePartTwo hasUsedJoker visited from to cave = do
  neighbours <- filterNeighbours <$> lookup from
  Branches <$> mapM launchNext neighbours
  where
    lookup node = case M.lookup (nodeName node) cave of
      Just node -> Right node
      Nothing -> Left $ "failed to find node " <> nodeName node
    launchNext (neighbour, usedJoker) = buildPathTreePartTwo usedJoker (from : visited) neighbour to cave <&> (nodeName neighbour,)
    filterNeighbours :: [Node] -> [(Node, Bool)]
    filterNeighbours = mapMaybe canBeVisitedAgain
      where
        canBeVisitedAgain :: Node -> Maybe (Node, Bool)
        canBeVisitedAgain node@(Big _) = Just (node, hasUsedJoker)
        canBeVisitedAgain node@(Small name) = case (hasUsedJoker, all (\n -> name /= nodeName n) visited) of
          (joker, True) -> Just (node, joker)
          (False, False) | name /= "start" -> Just (node, True)
          (False, False) -> Nothing
          (True, False) -> Nothing

pathTreePartTwo :: Cave -> Either String PathTree
pathTreePartTwo cave = buildPathTreePartTwo False [] (node "start") (node "end") cave <&> \tree -> Branches [("start", tree)]

pathTreeToList :: PathTree -> [[String]]
pathTreeToList Arrived = [[]]
pathTreeToList (Branches branches) =
  branches
    >>= (\(name, subTree) -> pathTreeToList subTree <&> (name :))

solvePartOne :: [Link] -> Either String Int
solvePartOne links = pathTree (buildCave links) <&> pathTreeToList <&> length

solvePartTwo :: [Link] -> Either String Int
solvePartTwo links = pathTreePartTwo (buildCave links) <&> pathTreeToList <&> length

partOne :: String -> Either String String
partOne input = parse input >>= solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input >>= solvePartTwo <&> show
