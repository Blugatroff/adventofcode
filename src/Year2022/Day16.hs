module Year2022.Day16 (partOne, partTwo) where

import MeLude
import Data.Map qualified as M
import Data.Set qualified as S
import Util (readInt, safeMaximum, split, trim, tuplePermutations)

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

type Name = (Char, Char)

data Valve = Valve {valveName :: Name, valveRate :: Int, valveTunnels :: [Valve]}

tupleToList :: (a, a) -> [a]
tupleToList (a, b) = [a, b]

instance Show Valve where
  show = tupleToList . valveName

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

type Path = [(Name, Name)]

type Paths = Map Name (Map Name Path)

type Network = Map Name Valve

getValve :: Name -> Network -> Valve
getValve name = fromMaybe (error "valve not found") . M.lookup name

findPaths :: Network -> Paths
findPaths network = findPathsFrom <$> network
  where
    findPathsFrom valve =
      let current = M.fromList [(valveName valve, [])]
       in go (current, current, M.empty)

    go :: (Map Name Path, Map Name Path, Map Name Path) -> Map Name Path
    go (current, connections, _) | M.null current = connections
    go (current, connections, _) = go (newNext, newConnections, M.empty)
      where
        (_, newConnections, newNext) = foldl fold (current, connections, M.empty) $ M.toList current

    fold :: (Map Name Path, Map Name Path, Map Name Path) -> (Name, Path) -> (Map Name Path, Map Name Path, Map Name Path)
    fold (current, connections, next) (name, path) = foldl fold (current, connections, next) $ valveTunnels (getValve name network)
      where
        fold :: (Map Name Path, Map Name Path, Map Name Path) -> Valve -> (Map Name Path, Map Name Path, Map Name Path)
        fold (current, connections, next) tunnel = case M.lookup (valveName tunnel) connections of
          Nothing ->
            let connPath = path <> [(name, valveName tunnel)]
             in (current, M.insert (valveName tunnel) connPath connections, M.insert (valveName tunnel) connPath next)
          Just _ -> (current, connections, next)

createNetwork :: [Valve] -> Map Name Valve
createNetwork = M.fromList . map (\valve -> (valveName valve, valve))

data Move = Move {moveReward :: Int, moveTarget :: Name, movePath :: Path}

moveCost :: Move -> Int
moveCost (Move {movePath}) = length movePath + 1

data State = State
  { stateNetwork :: Network,
    statePaths :: Paths,
    statePosition :: Name,
    stateMaxTurns :: Int,
    stateTurn :: Int,
    statePressure :: Int,
    stateOpenValves :: Set Name
  }

turnsLeft :: State -> Int
turnsLeft (State { stateMaxTurns, stateTurn }) = stateMaxTurns - stateTurn

getPaths :: Name -> Paths -> Map Name Path
getPaths src = fromMaybe M.empty . M.lookup src

findNextMoves :: State -> [Move]
findNextMoves state =
  getPaths (statePosition state) (statePaths state)
    & M.toList
    & mapMaybe (\(name, path) -> do
      when (S.member name (stateOpenValves state)) Nothing
      let flow = valveRate (getValve name (stateNetwork state))
      when (flow == 0) Nothing
      let travelTurns = length path
      let openTurns = 1
      let turnsSpentOpen = turnsLeft state - travelTurns - openTurns
      if turnsSpentOpen < 0 then Nothing
      else pure (Move (flow * turnsSpentOpen) name path)
    )

applyMove :: Move -> State -> State
applyMove move state = state
  { statePosition = moveTarget move
  , stateTurn = stateTurn state + moveCost move
  , statePressure = statePressure state + moveReward move
  , stateOpenValves = S.insert (moveTarget move) (stateOpenValves state)
  }

type ValvesOpen = Set Name
type BestPressureAchieved = Int
type Best = Map ValvesOpen BestPressureAchieved

applyBestMoves :: (State -> Best -> Best) -> State -> Best -> (State, [Move], Best)
applyBestMoves bestUpdater state best = spreadFrom [] state best $ findNextMoves state
  where
    spreadFrom :: [Move] -> State -> Best -> [Move] -> (State, [Move], Best)
    spreadFrom bestMoves bestState best = \case
      [] -> (bestState, bestMoves, bestUpdater state best)
      (move:moves) ->
        let next = applyMove move state
            (next', nextMoves, newBest) = applyBestMoves bestUpdater next best
        in if statePressure next' > statePressure bestState then
            spreadFrom nextMoves next' newBest moves
          else
            spreadFrom bestMoves bestState newBest moves


startingState :: Network -> Paths -> Int -> State
startingState network paths maxTurns = State network paths ('A', 'A') maxTurns 0 0 S.empty

solvePartOne :: Network -> Paths -> Int
solvePartOne network paths = statePressure $ (\(a, b, c) -> a) $ applyBestMoves (const id) (startingState network paths 30) M.empty

solvePartTwo :: Network -> Paths -> Int
solvePartTwo network paths = fromMaybe (-1) bestPressure
  where
    bestUpdater state = M.alter (\case
      Nothing -> Just (statePressure state)
      Just p | p < statePressure state -> Just (statePressure state)
      Just p -> Just p
      ) (stateOpenValves state)

    (_, _, best) = applyBestMoves bestUpdater (startingState network paths 26) M.empty

    bestPressure = M.toList best
      & tuplePermutations
      & filter (\((human, _), (elephant, _)) -> S.null $ S.intersection human elephant)
      & map (\((_, human), (_, elephant)) -> human + elephant)
      & safeMaximum

part :: (Network -> Paths -> Int) -> String -> Either String String
part solve input = do
  valves <- assembleValves <$> parse input
  let network = createNetwork valves
  let paths = findPaths network
  pure $ show $ solve network paths

partOne :: String -> Either String String
partOne = part solvePartOne

partTwo :: String -> Either String String
partTwo = part solvePartTwo
