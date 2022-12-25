module Year2022.Day16 (partOne, partTwo) where

import Data.Char (isDigit, isSpace, isUpper)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Set qualified as S
import Util (maximumByKey, minimumByKey, readInt, split, trim)

type RawValve = (String, Int, [String])

parseLine :: String -> Either String RawValve
parseLine line = do
  let splits = split ' ' line <&> filter (/= ',')
  let valve = splits !! 1
  let valves = splits & filter (all isUpper) & filter ((== 2) . length) & drop 1
  rate <- splits !! 4 & filter isDigit & readInt
  return (valve, rate, valves)

parse :: String -> Either String [RawValve]
parse input = split '\n' input <&> trim isSpace & filter (not . null) & traverse parseLine

data Step = Open Valve | MoveTo Valve | NoOp
  deriving (Eq, Ord)

data Valve = Valve {name :: (Char, Char), rate :: Int, tunnels :: [Valve]}

instance Eq Valve where
  a == b = name a == name b

instance Ord Valve where
  compare = compare `on` name

tupleToList :: (a, a) -> [a]
tupleToList (a, b) = [a, b]

instance Show Valve where
  show = tupleToList . name

instance Show Step where
  show (Open v) = "Open " <> tupleToList (name v)
  show (MoveTo v) = "MoveTo " <> tupleToList (name v)
  show NoOp = "NoOp"

assembleValves :: [RawValve] -> M.Map String Valve
assembleValves rawValves = map
  where
    map = M.fromList (rawValves <&> \rawValve@(name, _, _) -> (name, f rawValve))

    tupleName [a, b] = (a, b)
    tupleName _ = ('#', '#')

    f :: RawValve -> Valve
    f rawValue@(name, rate, tunnels) = Valve (tupleName name) rate $ do
      t <- tunnels
      maybeToList $ M.lookup t map

shortestPathBetween :: Valve -> Valve -> Maybe [Valve]
shortestPathBetween src dst = pathsBetween S.empty src dst & minimumByKey length
  where
    pathsBetween :: S.Set Valve -> Valve -> Valve -> [[Valve]]
    pathsBetween visited src dst | name src == name dst = [[]]
    pathsBetween visited src dst = indirect
      where
        indirect =
          tunnels src
            & filter notVisited
            <&> (\tunnel -> pathsBetween (S.insert src visited) tunnel dst <&> (tunnel :))
            & concat

        notVisited :: Valve -> Bool
        notVisited valve = not $ S.member valve visited

shortestPaths :: [Valve] -> Paths
shortestPaths valves = M.fromList $ do
    src <- valves
    dst <- valves
    path <- maybeToList $ shortestPathBetween src dst
    return ((src, dst), path)

type Paths = M.Map (Valve, Valve) [Valve]

simulatePlan :: [Step] -> Int
simulatePlan steps = foldl' runStep (0, 0) steps & snd
    where
        runStep (r, sum) NoOp = (r, sum + r)
        runStep (r, sum) (MoveTo valve) = (r, sum + r)
        runStep (r, sum) (Open valve) = (r + rate valve, sum + r)

findBestPlan :: [[Step]] -> Maybe [Step]
findBestPlan plans = plans <&> (\plan -> (plan, simulatePlan plan)) & maximumByKey snd <&> fst

allPlans :: Paths -> S.Set Valve -> Int -> Valve -> [[Step]]
allPlans paths openValves minute current | minute > 30 = [[]]
allPlans paths openValves minute current | S.null openValves = [[]]
allPlans paths openValves minute current = S.elems openValves >>= next
    where
        next valve = allPlans paths (S.delete valve openValves) (minute + length steps) valve
                <&> (steps ++)
            where
                steps = maybe [] (map MoveTo) (M.lookup (current, valve) paths) ++ [Open valve]

solvePartOne :: M.Map String Valve -> Maybe Int
solvePartOne valves = do
    startValve <- M.lookup "AA" valves
    let plans = make30Mins <$> allPlans paths (S.fromList nonNullValves) 0 startValve
    bestPlan <- findBestPlan plans
    return $ simulatePlan bestPlan
    where
        paths = shortestPaths $ M.elems valves
        nonNullValves = filter ((> 1) . rate) $ M.elems valves
        make30Mins plan = plan ++ repeat NoOp & take 30

solvePartTwo :: M.Map String Valve -> Maybe Int
solvePartTwo valves = do Nothing

partOne :: String -> Either String String
partOne input = parse input <&> assembleValves <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> assembleValves <&> solvePartTwo <&> show
