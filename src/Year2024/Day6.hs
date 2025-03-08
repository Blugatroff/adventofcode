module Year2024.Day6 (partOne, partTwo) where

import MeLude
import Data.Array.Base as Array
import Direction (Direction (..), turnRight)
import Util
import Data.Pos

type Tile = Word8 -- data Tile = Empty | Obstacle | Guard Direction deriving (Eq, Show)

empty, obstacle, guardUp, guardDown, guardLeft, guardRight :: Tile
empty = 0
obstacle = 1
guardUp = 2
guardDown = 3
guardLeft = 4
guardRight = 5

type LabMap = UArray Pos Tile

parse :: String -> Either String LabMap
parse = parseGrid parseTile
 where
  parseTile = \case
    '.' -> Right empty
    '#' -> Right obstacle
    '^' -> Right guardUp
    '>' -> Right guardRight
    '<' -> Right guardLeft
    'v' -> Right guardDown
    c -> Left $ "Failed to parse tile: " <> show c

findGuard :: LabMap -> Either String (Pos, Direction)
findGuard lab = case guards of
  [guard] -> Right guard
  _ -> Left "failed to find exactly one guard"
 where
  guards = mapMaybe extractGuard $ assocs lab
  extractGuard :: (Pos, Tile) -> Maybe (Pos, Direction)
  extractGuard (pos, tile) = case tile of
    tile | tile == guardUp -> Just (pos, DirUp)
    tile | tile == guardDown -> Just (pos, DirDown)
    tile | tile == guardLeft -> Just (pos, DirLeft)
    tile | tile == guardRight -> Just (pos, DirRight)
    _ -> Nothing

walk :: LabMap -> Direction -> Pos -> Maybe [Pos]
walk lab dir pos = runST $ do
  let (min, max) = bounds lab
  visited <-
    newArray ((min, minBound), (max, maxBound)) False ::
      ST r (STUArray r (Pos, Direction) Bool)
  let go path dir pos = do
        let pos' = pos + Pos dir.x dir.y
        case inRange (bounds lab) pos' of
          False -> pure $ Just path
          True | lab ! pos' == obstacle -> do
            let posDir = (pos', dir)
            readArray visited posDir >>= \case
              True -> do
                pure Nothing
              False -> do
                writeArray visited posDir True
                go path (turnRight dir) pos
          True -> go (pos' : path) dir pos'
  go [] dir pos

partOne :: String -> Either String String
partOne input = do
  lab <- parse input
  (pos, dir) <- findGuard lab
  path <- maybeToEither "Cycle detected" $ walk lab dir pos
  Right $ show $ length $ dedup $ pos : path

partTwo :: String -> Either String String
partTwo input = do
  lab <- parse input
  (guardPos, dir) <- findGuard lab
  path <- maybeToEither "Cycle detected" $ walk lab dir guardPos
  Right $ show $ length $ parFilter id $ do
    pos <- fst <$> dedup path
    guard (pos /= guardPos)
    guard (lab ! pos /= obstacle)
    let modifiedLab = lab // [(pos, obstacle)]
    pure $ isNothing $ walk modifiedLab dir guardPos
