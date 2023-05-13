module Year2022.Day19 (partOne, partTwo) where

import Data.Array as Array
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.Heap qualified as Heap
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Util (readInt, rightToMaybe, split, trim)

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

data Resource = Ore | Clay | Obsidian | Geode
  deriving (Show, Eq, Ord)

allResources :: [Resource]
allResources = [Ore, Clay, Obsidian, Geode]

newtype Robot = Robot Resource
  deriving (Show, Eq, Ord)

allRobots :: [Robot]
allRobots = map Robot allResources

data Action = DoNothing | Build Robot
  deriving (Eq, Ord)

instance Show Action where
  show DoNothing = "None"
  show (Build (Robot Ore)) = " Ore "
  show (Build (Robot Clay)) = "Clay"
  show (Build (Robot Obsidian)) = "Obsi"
  show (Build (Robot Geode)) = "Geod"

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
    geodesProduced :: Int,
    previousActions :: [Action]
  }
  deriving (Eq, Show)

fact :: Int -> Int
fact n | n < 0 = 0
fact n = n + fact (n - 1)

facts :: Array.Array Int Int
facts = Array.array (0, 50) [(i, fact i) | i <- [0 .. 50]]

fastFact :: Int -> Int
fastFact n | n < 0 = 0
fastFact n = facts ! n

geodeRobot :: Robot
geodeRobot = Robot Geode

testEnoughResources :: Robot -> State -> Bool
testEnoughResources (Robot Ore) (State (Blueprint {oreRobotCost}) state _ _) = state.availableOre >= oreRobotCost.ore
testEnoughResources (Robot Clay) (State (Blueprint {clayRobotCost}) state _ _) = state.availableOre >= clayRobotCost.ore
testEnoughResources (Robot Obsidian) (State (Blueprint {obsidianRobotCost}) state _ _) = state.availableOre >= obsidianRobotCost.ore && state.availableClay >= obsidianRobotCost.clay
testEnoughResources (Robot Geode) (State (Blueprint {geodeRobotCost}) state _ _) = state.availableOre >= geodeRobotCost.ore && state.availableObsidian >= geodeRobotCost.obsidian

useResources :: Robot -> State -> Maybe State
useResources (Robot Ore) (State b@(Blueprint {oreRobotCost}) state p a) = if state.availableOre < oreRobotCost.ore then Nothing else Just $ State b (state {availableOre = state.availableOre - oreRobotCost.ore}) p a
useResources (Robot Clay) (State b@(Blueprint {clayRobotCost}) state p a) = if state.availableOre < clayRobotCost.ore then Nothing else Just $ State b (state {availableOre = state.availableOre - clayRobotCost.ore}) p a
useResources (Robot Obsidian) (State b@(Blueprint {obsidianRobotCost}) state p a) = if state.availableOre < obsidianRobotCost.ore || state.availableClay < obsidianRobotCost.clay then Nothing else Just $ State b (state {availableOre = state.availableOre - obsidianRobotCost.ore, availableClay = state.availableClay - obsidianRobotCost.clay}) p a
useResources (Robot Geode) (State b@(Blueprint {geodeRobotCost}) state p a) = if state.availableOre < geodeRobotCost.ore || state.availableObsidian < geodeRobotCost.obsidian then Nothing else Just $ State b (state {availableOre = state.availableOre - geodeRobotCost.ore, availableObsidian = state.availableObsidian - geodeRobotCost.obsidian}) p a

mostPossibleGeodes :: State -> Int
mostPossibleGeodes state@(State _ s p _) = estimateWithoutNewRobots + producedFromNewRobots
  where
    estimateWithoutNewRobots = p + (s.minutesLeft * s.geodeRobots)
    -- if a new geode robot would be built every minute
    producedFromNewRobots = if testEnoughResources geodeRobot state then fastFact s.minutesLeft else fastFact (s.minutesLeft - 1)

instance Ord State where
  compare ls@(State _ l _ _) rs@(State _ r _ _) = case compare (mostPossibleGeodes ls) (mostPossibleGeodes rs) of
    EQ -> compare l.minutesLeft r.minutesLeft
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
      geodesProduced = 0,
      previousActions = []
    }

simulate :: State -> Action -> Maybe State
simulate state action = simulateFactory $ advanceTime $ appendAction state
  where
    collectMinerals (State b state p a) =
      State
        b
        ( state
            { availableOre = state.availableOre + state.oreRobots,
              availableClay = state.availableClay + state.clayRobots,
              availableObsidian = state.availableObsidian + state.obsidianRobots,
              availableGeodes = state.availableGeodes + state.geodeRobots
            }
        )
        (p + state.geodeRobots)
        a

    advanceTime (State b state p a) = State b (state {minutesLeft = state.minutesLeft - 1}) p a
    appendAction (State b state p actions) = State b state p (action : actions)

    simulateFactory = case action of
      DoNothing -> Just . collectMinerals
      Build robot -> fmap (addRobot robot . collectMinerals) . useResources robot

    addRobot (Robot Ore) (State b state p a) = State b (state {oreRobots = state.oreRobots + 1}) p a
    addRobot (Robot Clay) (State b state p a) = State b (state {clayRobots = state.clayRobots + 1}) p a
    addRobot (Robot Obsidian) (State b state p a) = State b (state {obsidianRobots = state.obsidianRobots + 1}) p a
    addRobot (Robot Geode) (State b state p a) = State b (state {geodeRobots = state.geodeRobots + 1}) p a

allActions :: [Action]
allActions = map Build (reverse allRobots) <> [DoNothing]

tryActionIfUseful :: State -> Action -> Maybe State
tryActionIfUseful state@(State {blueprint = Blueprint {maxOreCost, maxClayCost, maxObsidianCost}, inner = s}) = \case
  (Build (Robot Ore)) | s.oreRobots >= maxOreCost -> Nothing
  (Build (Robot Clay)) | s.clayRobots >= maxClayCost -> Nothing
  (Build (Robot Obsidian)) | s.obsidianRobots >= maxObsidianCost -> Nothing
  action -> simulate state action

newtype ReversedOrd a = ReversedOrd a
  deriving (Eq)

instance Ord a => Ord (ReversedOrd a) where
  compare (ReversedOrd a) (ReversedOrd b) = compare b a

solveBlueprint :: Int -> Blueprint -> State
solveBlueprint minutesLeft blueprint = f S.empty 0 (Heap.singleton (ReversedOrd init))
  where
    init = initialState blueprint minutesLeft

    f :: S.Set InnerState -> Int -> Heap.Heap (ReversedOrd State) -> State
    f seen mostGeodes queue = case Heap.viewMin queue of
      Nothing -> init
      Just (ReversedOrd state@(State _ s _ _), _) | s.minutesLeft == 0 -> state
      Just (ReversedOrd (State _ s _ _), queue) | S.member s seen -> f seen mostGeodes queue
      Just (ReversedOrd state@(State _ s p _), queue) ->
        let newSeen = S.insert s seen
            newMostGeodes = max mostGeodes p
            tryActionIfUsefulHere = tryActionIfUseful state
            folding queue action = forEachPossibleState newMostGeodes seen queue (tryActionIfUsefulHere action)
            newQueue = foldl folding queue allActions
         in f newSeen newMostGeodes newQueue

    forEachPossibleState :: Int -> S.Set InnerState -> Heap.Heap (ReversedOrd State) -> Maybe State -> Heap.Heap (ReversedOrd State)
    forEachPossibleState _ _ queue Nothing = queue
    forEachPossibleState _ seen queue (Just nextState) | S.member nextState.inner seen = queue
    forEachPossibleState mostGeodes _ queue (Just nextState) | mostPossibleGeodes nextState < mostGeodes = queue
    forEachPossibleState _ _ queue (Just nextState) = Heap.insert (ReversedOrd nextState) queue

solvePartOne :: [Blueprint] -> Int
solvePartOne blueprints = sum $ map scoreBlueprint blueprints
  where
    scoreBlueprint blueprint@(Blueprint {id}) = id * (solveBlueprint 24 blueprint).geodesProduced

solvePartTwo :: [Blueprint] -> Int
solvePartTwo blueprints = product $ map ((.geodesProduced) . solveBlueprint 32) $ take 3 blueprints

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
