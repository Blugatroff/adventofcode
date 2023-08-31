module Year2022.Day24 (partOne, partTwo) where

import Control.Arrow ((>>>))
import Data.Char (isSpace)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.List (singleton)
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Traversable (for)
import Dijkstra
import Direction
import Util (safeHead, safeLast, split, trim)

type Pos = (Int, Int)

data Valley = Valley
  { blizzards :: M.Map Pos [Direction], -- every position not in the map is a wall
    start :: Pos,
    dest :: Pos,
    end :: Pos,
    minute :: Int
  }
  deriving (Eq)

parse :: String -> Either String Valley
parse input = do
  lines <- for (zip [0 ..] $ filter (not . null) $ map (trim isSpace) $ split '\n' input) $ \(y, line) ->
    catMaybes <$> do
      for (zip [0 ..] line) $ \(x, c) ->
        fmap ((x, y),) <$> do
          case c of
            '#' -> Right Nothing
            '.' -> Right $ Just []
            '^' -> Right $ Just [DirUp]
            '>' -> Right $ Just [DirRight]
            'v' -> Right $ Just [DirDown]
            '<' -> Right $ Just [DirLeft]
            c -> Left $ "Failed too parse tile " <> show c
  start <- maybeToEither "Failed to find start" $ fmap fst (safeHead lines >>= find (snd >>> (== [])))
  dest <- maybeToEither "Failed to find end" $ fmap fst (safeLast lines >>= find (snd >>> (== [])))
  let assocs = concat lines
  let blizzards = M.fromList assocs
  let end = (fst dest + 1, snd dest)
  pure $ Valley {start, dest, blizzards, end, minute = 0}

moveBlizzards :: Valley -> Valley
moveBlizzards valley = valley {blizzards = newBlizzards, minute = valley.minute + 1}
  where
    newBlizzards = foldl fold M.empty newBlizzardPositions

    fold m (pos, blizzards) = M.insertWith (<>) pos blizzards m

    newBlizzardPositions :: [(Pos, [Direction])]
    newBlizzardPositions = do
      (pos, blizzards) <- M.toList valley.blizzards
      case blizzards of
        [] -> pure (pos, [])
        blizzards -> moveBlizzard pos =<< blizzards

    moveBlizzard pos@(x, y) blizzard = [(pos, []), (wrapPos newPos, [blizzard])]
      where
        newPos = (x + directionX blizzard, y + directionY blizzard)

        wrapPos (x, y) | x <= 0 = wrapPos (fst valley.end - 1, y)
        wrapPos (x, y) | y <= 0 = wrapPos (x, snd valley.end - 1)
        wrapPos (x, y) | x >= fst valley.end = wrapPos (1, y)
        wrapPos (x, y) | y >= snd valley.end = wrapPos (x, 1)
        wrapPos pos = pos

data Action = Wait | Move Direction

possibleActions :: Valley -> Pos -> [Action]
possibleActions valley pos@(x, y) = moveActions <> waitAction
  where
    moveActions =
      mapMaybe
        ( \dir -> do
            let newPos = (x + directionX dir, y + directionY dir)
            blizzards <- M.lookup newPos valley.blizzards
            case blizzards of
              [] -> Just $ Move dir
              _ -> Nothing
        )
        allDirections

    waitAction = case M.lookup pos valley.blizzards of
      Nothing -> []
      Just [] -> [Wait]
      Just _ -> []

data Phase = ToGoalFirst | BackToStart | ToGoalSecond
  deriving (Eq, Ord)

data ValleyWorld = ValleyWorld

data ValleyWorldPos = ValleyWorldPos Phase Pos [Valley]
  deriving (Eq)

instance Ord ValleyWorldPos where
  compare (ValleyWorldPos phaseL posL valleysL) (ValleyWorldPos phaseR posR valleysR) = case compare phaseL phaseR of
    EQ -> case compare posL posR of
      EQ -> compare (minute <$> safeHead valleysL) (minute <$> safeHead valleysR)
      c -> c
    c -> c

instance World ValleyWorld ValleyWorldPos where
  lookupCell (ValleyWorldPos phase pos valleys) ValleyWorld = do
    valley <- safeHead valleys
    _ <- M.lookup pos valley.blizzards
    case phase of
      ToGoalSecond | valley.dest == pos -> Just $ Destination 1
      _ -> Just $ Cell 1

  adjacentCells (ValleyWorldPos phase pos@(x, y) valleys) ValleyWorld = do
    valley <- maybe [] singleton $ safeHead valleys
    let nextValleys = tail valleys
    possibleActions valley pos <&> \case
      Wait -> ValleyWorldPos phase pos nextValleys
      Move direction -> do
        let newPos = (x + directionX direction, y + directionY direction)
        case phase of
          ToGoalFirst | valley.dest == newPos -> ValleyWorldPos BackToStart newPos nextValleys
          BackToStart | valley.start == newPos -> ValleyWorldPos ToGoalSecond newPos nextValleys
          phase -> ValleyWorldPos phase newPos nextValleys

solve :: Phase -> Valley -> String
solve startPhase valley = do
  let valleys = iterate moveBlizzards valley
  let startPos = ValleyWorldPos startPhase valley.start valleys
  let solution = findSolutionFrom ValleyWorld startPos
  maybe "No Solution Found!" (show . subtract 2 . cost) solution

partOne :: String -> Either String String
partOne = fmap (solve ToGoalSecond) <$> parse

partTwo :: String -> Either String String
partTwo = fmap (solve ToGoalFirst) <$> parse
