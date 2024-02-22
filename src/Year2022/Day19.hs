module Year2022.Day19 (partOne, partTwo) where

import Control.Parallel.Strategies
import Data.Char (isSpace)
import Data.Heap qualified as Heap
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Util (nthTriangle, readInt, rightToMaybe, split, trim)

newtype OreCost = OreCost {ore :: Int} deriving (Eq, Show)

data OreClayCost = OreClayCost {ore :: !Int, clay :: !Int} deriving (Eq, Show)

data OreObsidianCost = OreObsidianCost {ore :: !Int, obsidian :: !Int} deriving (Eq, Show)

data Blueprint = Blueprint
  { id :: !Int,
    oreRobotCost :: !OreCost,
    clayRobotCost :: !OreCost,
    obsidianRobotCost :: !OreClayCost,
    geodeRobotCost :: !OreObsidianCost,
    maxOreCost :: !Int,
    maxClayCost :: !Int,
    maxObsidianCost :: !Int
  }
  deriving (Eq, Show)

parseBlueprint :: String -> Either String Blueprint
parseBlueprint line = do
  let splits = map (trim isSpace) $ split ':' =<< split ' ' line
  let numbers = mapMaybe (rightToMaybe . readInt) splits
  case numbers of
    [id, oreRobotOreCost, clayRobotOreCost, obsidianRobotOreCost, obsidianRobotClayCost, geodeRobotOreCost, geodeRobotObsidianCost] ->
      Right $
        Blueprint
          { id,
            oreRobotCost = OreCost {ore = oreRobotOreCost},
            clayRobotCost = OreCost {ore = clayRobotOreCost},
            obsidianRobotCost = OreClayCost {ore = obsidianRobotOreCost, clay = obsidianRobotClayCost},
            geodeRobotCost = OreObsidianCost {ore = geodeRobotOreCost, obsidian = geodeRobotObsidianCost},
            maxOreCost = oreRobotOreCost `max` clayRobotOreCost `max` obsidianRobotOreCost `max` geodeRobotOreCost,
            maxClayCost = obsidianRobotClayCost,
            maxObsidianCost = geodeRobotObsidianCost
          }
    _ -> Left $ "Failed to parse blueprint: " <> line

parse :: String -> Either String [Blueprint]
parse = traverse parseBlueprint . filter (not . null) . map (trim isSpace) . lines

data Resource = Ore | Clay | Obsidian | Geode deriving (Show, Eq, Ord)

allResources :: [Resource]
allResources = [Ore, Clay, Obsidian, Geode]

newtype Robot = Robot Resource deriving (Show, Eq, Ord)

allRobots :: [Robot]
allRobots = map Robot allResources

data Action = DoNothing | Build Robot deriving (Eq, Ord)

data InnerState = InnerState
  { minutesLeft :: Int,
    geodeRobots :: Int,
    obsidianRobots :: Int,
    clayRobots :: Int,
    oreRobots :: Int,
    availableGeodes :: Int,
    availableObsidian :: Int,
    availableClay :: Int,
    availableOre :: Int
  }
  deriving (Eq, Ord, Show)

data State = State
  { blueprint :: Blueprint,
    inner :: InnerState,
    geodesProduced :: Int
  }
  deriving (Eq, Show)

geodeRobot :: Robot
geodeRobot = Robot Geode

testEnoughResources :: Robot -> State -> Bool
testEnoughResources (Robot Ore) State {blueprint = Blueprint {oreRobotCost}, inner = state} = state.availableOre >= oreRobotCost.ore
testEnoughResources (Robot Clay) State {blueprint = Blueprint {clayRobotCost}, inner = state} = state.availableOre >= clayRobotCost.ore
testEnoughResources (Robot Obsidian) State {blueprint = Blueprint {obsidianRobotCost}, inner = state} = state.availableOre >= obsidianRobotCost.ore && state.availableClay >= obsidianRobotCost.clay
testEnoughResources (Robot Geode) State {blueprint = Blueprint {geodeRobotCost}, inner = state} = state.availableOre >= geodeRobotCost.ore && state.availableObsidian >= geodeRobotCost.obsidian

useResources :: Robot -> State -> Maybe State
useResources robot s@State {blueprint = b@Blueprint {oreRobotCost, clayRobotCost, obsidianRobotCost, geodeRobotCost}, inner = state} = case robot of
  Robot Ore | state.availableOre < oreRobotCost.ore -> Nothing
  Robot Ore -> Just $ s {inner = state {availableOre = state.availableOre - oreRobotCost.ore}}
  Robot Clay | state.availableOre < clayRobotCost.ore -> Nothing
  Robot Clay -> Just $ s {inner = state {availableOre = state.availableOre - clayRobotCost.ore}}
  Robot Obsidian | state.availableOre < obsidianRobotCost.ore || state.availableClay < obsidianRobotCost.clay -> Nothing
  Robot Obsidian -> Just $ s {inner = state {availableOre = state.availableOre - obsidianRobotCost.ore, availableClay = state.availableClay - obsidianRobotCost.clay}}
  Robot Geode | state.availableOre < geodeRobotCost.ore || state.availableObsidian < geodeRobotCost.obsidian -> Nothing
  Robot Geode -> Just $ s {inner = state {availableOre = state.availableOre - geodeRobotCost.ore, availableObsidian = state.availableObsidian - geodeRobotCost.obsidian}}

mostPossibleGeodes :: State -> Int
mostPossibleGeodes state@(State _ s p) = estimateWithoutNewRobots + producedFromNewRobots
  where
    estimateWithoutNewRobots = p + (s.minutesLeft * s.geodeRobots)
    -- if a new geode robot would be built every minute
    producedFromNewRobots = if testEnoughResources geodeRobot state then nthTriangle s.minutesLeft else nthTriangle (s.minutesLeft - 1)

instance Ord State where
  compare ls@(State _ l _) rs@(State _ r _) = case compare (mostPossibleGeodes ls) (mostPossibleGeodes rs) of
    EQ -> case compare l.minutesLeft r.minutesLeft of
      EQ -> compare l.availableObsidian r.availableObsidian
      r -> r
    r -> r

initialState :: Blueprint -> Int -> State
initialState blueprint minutesLeft =
  State
    { blueprint,
      inner =
        InnerState
          { minutesLeft,
            availableOre = 0,
            availableClay = 0,
            availableObsidian = 0,
            availableGeodes = 0,
            oreRobots = 1,
            clayRobots = 0,
            obsidianRobots = 0,
            geodeRobots = 0
          },
      geodesProduced = 0
    }

collectMinerals :: State -> State
collectMinerals state =
  state
    { inner =
        state.inner
          { availableOre = state.inner.availableOre + state.inner.oreRobots,
            availableClay = state.inner.availableClay + state.inner.clayRobots,
            availableObsidian = state.inner.availableObsidian + state.inner.obsidianRobots,
            availableGeodes = state.inner.availableGeodes + state.inner.geodeRobots
          },
      geodesProduced = state.geodesProduced + state.inner.geodeRobots
    }

simulate :: State -> Action -> Maybe State
simulate state action = simulateFactory $ advanceTime state
  where
    advanceTime state@(State {inner}) = state {inner = state.inner {minutesLeft = state.inner.minutesLeft - 1}}
    simulateFactory = case action of
      DoNothing -> Just . collectMinerals
      Build robot -> fmap (addRobot robot . collectMinerals) . useResources robot

    addRobot (Robot Ore) state@State {inner} = state {inner = state.inner {oreRobots = state.inner.oreRobots + 1}}
    addRobot (Robot Clay) state@State {inner} = state {inner = state.inner {clayRobots = state.inner.clayRobots + 1}}
    addRobot (Robot Obsidian) state@State {inner} = state {inner = state.inner {obsidianRobots = state.inner.obsidianRobots + 1}}
    addRobot (Robot Geode) state@State {inner} = state {inner = state.inner {geodeRobots = state.inner.geodeRobots + 1}}

allActions :: [Action]
allActions = map Build (reverse allRobots) <> [DoNothing]

tryActionIfUseful :: State -> Action -> Maybe State
tryActionIfUseful state@(State {blueprint = Blueprint {maxOreCost, maxClayCost, maxObsidianCost}, inner = s}) = \case
  (Build (Robot Ore)) | s.oreRobots >= maxOreCost -> Nothing
  (Build (Robot Clay)) | s.clayRobots >= maxClayCost -> Nothing
  (Build (Robot Obsidian)) | s.obsidianRobots >= maxObsidianCost -> Nothing
  action -> simulate state action

newtype ReversedOrd a = ReversedOrd a deriving (Eq)

instance (Ord a) => Ord (ReversedOrd a) where
  compare (ReversedOrd a) (ReversedOrd b) = compare b a

solveBlueprint :: Int -> Blueprint -> State
solveBlueprint minutesLeft blueprint = f S.empty 0 (Heap.singleton (ReversedOrd init))
  where
    init = initialState blueprint minutesLeft

    f :: S.Set InnerState -> Int -> Heap.Heap (ReversedOrd State) -> State
    f seen mostGeodes queue = case Heap.viewMin queue of
      Nothing -> init
      Just (ReversedOrd s@(State {inner = state}), _) | state.minutesLeft == 0 -> s
      Just (ReversedOrd State {inner = state}, queue) | S.member state seen -> f seen mostGeodes queue
      Just (ReversedOrd s@State {inner = state, geodesProduced}, queue) ->
        let newSeen = S.insert state seen
            newMostGeodes = max mostGeodes geodesProduced
            tryActionIfUsefulHere = tryActionIfUseful s
            folding queue action = forEachPossibleState newMostGeodes seen queue (tryActionIfUsefulHere action)
            newQueue = foldl' folding queue allActions
         in f newSeen newMostGeodes newQueue

    forEachPossibleState :: Int -> S.Set InnerState -> Heap.Heap (ReversedOrd State) -> Maybe State -> Heap.Heap (ReversedOrd State)
    forEachPossibleState _ _ queue Nothing = queue
    forEachPossibleState _ seen queue (Just nextState) | S.member nextState.inner seen = queue
    forEachPossibleState mostGeodes _ queue (Just nextState) | mostPossibleGeodes nextState <= mostGeodes = queue
    forEachPossibleState _ _ queue (Just nextState) = Heap.insert (ReversedOrd nextState) queue

solvePartOne :: [Blueprint] -> Int
solvePartOne = sum . (`using` parList rdeepseq) . map scoreBlueprint
  where
    scoreBlueprint blueprint@(Blueprint {id}) = id * (solveBlueprint 24 blueprint).geodesProduced

solvePartTwo :: [Blueprint] -> Int
solvePartTwo = product . (`using` parList rdeepseq) . map ((.geodesProduced) . solveBlueprint 32) . take 3

partOne :: String -> Either String String
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input
