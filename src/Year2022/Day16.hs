module Year2022.Day16 (partOne, partTwo) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.DeepSeq (NFData, deepseq)
import Control.Exception (BlockedIndefinitelyOnMVar, try)
import Control.Monad (forM_)
import Data.Char (isDigit, isSpace, isUpper)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe (catMaybes, maybeToList)
import Data.Set qualified as S
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import Util (mapFst, mapSnd, minimumByKey, readInt, safeMaximum, split, trim)

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
  deriving (Eq, Ord, Generic)

instance NFData Step

data Valve = Valve {name :: (Char, Char), rate :: Int, tunnels :: [Valve]}
  deriving (Generic)

instance NFData Valve

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

assembleValves :: [RawValve] -> [Valve]
assembleValves rawValves = M.elems valveMap
  where
    valveMap = M.fromList (rawValves <&> \rawValve@(name, _, _) -> (name, f rawValve))

    tupleName [a, b] = (a, b)
    tupleName _ = ('#', '#')

    f :: RawValve -> Valve
    f rawValue@(name, rate, tunnels) = Valve (tupleName name) rate $ do
      t <- tunnels
      maybeToList $ M.lookup t valveMap

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

readChannelUntilEmpty :: Chan a -> IO [a]
readChannelUntilEmpty chan = do
  result <- try $ readChan chan
  case result of
    Left (e :: BlockedIndefinitelyOnMVar) -> return []
    Right v -> (v :) <$> readChannelUntilEmpty chan

parallelMaximum :: (NFData a, Ord a) => [[a]] -> Maybe a
parallelMaximum chunks = unsafePerformIO $ do
  chan <- newChan
  forM_ chunks $ \chunk -> do
    forkIO $ do
      let max = safeMaximum chunk
      max `deepseq` writeChan chan max
  chunkMaximums <- readChannelUntilEmpty chan
  return $ safeMaximum $ catMaybes chunkMaximums

solvePartOne :: [Valve] -> Maybe Int
solvePartOne valves = do
  startValve <- M.lookup ('A', 'A') valveMap
  let plans = map (simulatePlan . make30Mins) <$> allPlans paths (S.fromList nonNullValves) 0 startValve
  parallelMaximum plans
  where
    valveMap = M.fromList $ (\valve -> (name valve, valve)) <$> valves
    paths = shortestPaths valves
    nonNullValves = filter ((> 1) . rate) valves
    make30Mins plan = plan ++ repeat NoOp & take 30

    simulatePlan :: [Step] -> Int
    simulatePlan steps = foldl' runStep (0, 0) steps & snd
      where
        runStep (r, sum) NoOp = (r, sum + r)
        runStep (r, sum) (MoveTo valve) = (r, sum + r)
        runStep (r, sum) (Open valve) = (r + rate valve, sum + r)

    allPlans :: Paths -> S.Set Valve -> Int -> Valve -> [[[Step]]]
    allPlans paths openValves minute current | minute > 30 = [[[]]]
    allPlans paths openValves minute current | S.null openValves = [[[]]]
    allPlans paths openValves minute current = S.elems openValves <&> next
      where
        next valve = (steps ++) <$> concat (allPlans paths (S.delete valve openValves) (minute + length steps) valve)
          where
            steps = maybe [] (map MoveTo) (M.lookup (current, valve) paths) ++ [Open valve]

solvePartTwo :: [Valve] -> Maybe Int
solvePartTwo valves = do
  startValve <- M.lookup ('A', 'A') valveMap
  let plans = map (simulatePlanWithElefant . makeMins 26 (NoOp, NoOp)) <$> allPlansWithElefant paths (S.fromList nonNullValves) 0 ([], []) (startValve, startValve)
  parallelMaximum plans
  where
    valveMap = M.fromList $ (\valve -> (name valve, valve)) <$> valves
    paths = shortestPaths valves
    nonNullValves = filter ((> 1) . rate) valves
    makeMins time def plan = plan ++ repeat def & take time

    simulatePlanWithElefant :: [(Step, Step)] -> Int
    simulatePlanWithElefant steps = snd $ foldl' runStep (0, 0) steps
      where
        runStep (r, sum) (a, b) = executeStep a $ executeStep b $ timeFlow (r, sum)

        timeFlow (r, sum) = (r, sum + r)

        executeStep NoOp (r, sum) = (r, sum)
        executeStep (MoveTo valve) (r, sum) = (r, sum)
        executeStep (Open valve) (r, sum) = (r + rate valve, sum)

    allPlansWithElefant :: Paths -> S.Set Valve -> Int -> ([Step], [Step]) -> (Valve, Valve) -> [[[(Step, Step)]]]
    allPlansWithElefant paths openValves minute queued current | minute > 26 = [[[]]]
    allPlansWithElefant paths openValves minute queued current@(currentElf, currentElefant) = case queued of
      ([], []) | S.null openValves -> [[[]]]
      (elfStep : queuedElfSteps, elefantStep : queuedElefantSteps) ->
        (openValves, current)
          & applyStep mapFst elfStep
          & applyStep mapSnd elefantStep
          & \(openValves, current) ->
            map ((elfStep, elefantStep) :) <$> allPlansWithElefant paths openValves (minute + 1) (queuedElfSteps, queuedElefantSteps) current
      ([], queuedElefantSteps) -> generateNewSteps (,queuedElefantSteps) fst queuedElefantSteps
      (queuedElfSteps, []) -> generateNewSteps (queuedElfSteps,) snd queuedElfSteps
      where
        applyStep which NoOp (openValves, current) = (openValves, current)
        applyStep which (MoveTo valve) (openValves, current) = (openValves, which (const valve) current)
        applyStep which (Open valve) (openValves, current) = (S.delete valve openValves, current)

        nextQueue current valve = maybe [] (map MoveTo) (M.lookup (current, valve) paths) ++ [Open valve]

        notAlreadyOpenedByPartner partnerSteps valve = Open valve `notElem` partnerSteps

        generateNewSteps build which partnerSteps =
          if null nonReservedOpenValves
            then allPlansWithElefant paths openValves minute (build [NoOp]) current
            else nonReservedOpenValves <&> (\valve -> concat $ allPlansWithElefant paths openValves minute (build (nextQueue (which current) valve)) current)
          where
            nonReservedOpenValves = S.elems openValves & filter (notAlreadyOpenedByPartner partnerSteps)

partOne :: String -> Either String String
partOne input = parse input <&> assembleValves <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> assembleValves <&> solvePartTwo <&> show
