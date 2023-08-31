module Year2022.Day22 (partOne, partTwo) where

import Control.Arrow ((>>>))
import Control.Monad (zipWithM, (>=>))
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Either.Extra (maybeToEither)
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.List.Extra (dropEnd)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Traversable (for)
import Util (applyN, maximumByKey, maximumOrZero, minimumByKey, minimumOrZero, readInt, safeLast, safeMaximum, safeMinimum, splitSeq, trim)
import Direction

data Instruction = TurnLeft | TurnRight | Move Int
  deriving (Show)

type Path = [Instruction]

data Tile = Air | Wall
  deriving (Show, Eq)

type Board = M.Map (Int, Int) Tile

parse :: String -> Either String (Board, Path)
parse input = do
  let trimIfEmptpyAfterTrim s = if null (trim isSpace s) then trim isSpace s else s
  let theLines = filter (not . null) $ map trimIfEmptpyAfterTrim $ lines input
  path <- maybeToEither "failed to parse empty input" $ safeLast theLines
  path <- parsePath path
  board <- parseBoard $ filter (not . null) $ dropEnd 1 theLines
  pure (board, path)

parseBoard :: [String] -> Either String Board
parseBoard lines =
  M.fromList . concat <$> do
    let parseLine y line = do
          let indexedTiles = zip [0 ..] line
          let indexedTilesWithoutSpaces = filter ((/= ' ') . snd) indexedTiles
          for indexedTilesWithoutSpaces $ \(x, tile) -> do
            tile <- parseTile tile
            pure ((x, y), tile)
    zipWithM parseLine [0 ..] lines

parseTile :: Char -> Either String Tile
parseTile '.' = Right Air
parseTile '#' = Right Wall
parseTile tile = Left $ "failed to parse tile: " <> show tile

parsePath :: String -> Either String Path
parsePath =
  traverse parseInstruction
    . filter (not . null)
    . (splitKeepDelim "R" >=> splitKeepDelim "L")

splitKeepDelim :: String -> String -> [String]
splitKeepDelim pattern = intersperse pattern . splitSeq pattern

parseInstruction :: String -> Either String Instruction
parseInstruction "L" = Right TurnLeft
parseInstruction "R" = Right TurnRight
parseInstruction n = fmap Move $ first (\_ -> "failed to parse instruction: '" <> n <> "'") $ readInt n

data Cursor = Cursor {direction :: Direction, pos :: (Int, Int)}

executeInstruction :: Board -> Cursor -> Instruction -> Cursor
executeInstruction _ (Cursor direction pos) TurnLeft = Cursor (turnLeft direction) pos
executeInstruction _ (Cursor direction pos) TurnRight = Cursor (turnRight direction) pos
executeInstruction board cursor (Move distance) = applyN distance (moveCursor board) cursor

moveCursor :: Board -> Cursor -> Cursor
moveCursor board cursor@(Cursor direction pos@(px, py)) = case M.lookup newPos board of
  Nothing -> Cursor direction (fromMaybe pos $ wrap cursor)
  Just Air -> Cursor direction newPos
  Just Wall -> cursor
  where
    newX = px + directionX cursor.direction
    newY = py + directionY cursor.direction
    newPos = (newX, newY)

    extremes :: Ord b => (a -> b) -> [a] -> Maybe a
    extremes = case cursor.direction of
      DirRight -> minimumByKey
      DirLeft -> maximumByKey
      DirDown -> minimumByKey
      DirUp -> maximumByKey

    wrap cursor = do
      let axisCompare = directionAxis cursor.direction . fst
      let tileOnPlane (pos, tile) = case (tile :: Tile) of
            Air | onPlane pos -> Just (pos, Air)
            Wall | onPlane pos -> Just (pos, Wall)
            _ -> Nothing
      let tiles = mapMaybe tileOnPlane $ M.toList board
      (pos, tile) <- extremes axisCompare tiles
      if tile == Air then Just pos else Nothing

    onPlane (x, y) = case cursor.direction of
      DirRight -> y == py
      DirLeft -> y == py
      DirDown -> x == px
      DirUp -> x == px

findStart :: Board -> Maybe (Int, Int)
findStart board = do
  y <- safeMinimum $ map (snd . fst) $ M.toList board
  x <- safeMinimum $ map fst $ filter ((== y) . snd) $ map fst $ M.toList board
  pure (x, y)

score :: Cursor -> Int
score (Cursor {direction, pos = (column, row)}) = directionScore direction + 1000 * (row + 1) + 4 * (column + 1)

directionScore :: Direction -> Int
directionScore = \case
  DirRight -> 0
  DirDown -> 1
  DirLeft -> 2
  DirUp -> 3

solvePartOne :: (Board, Path) -> Either String Int
solvePartOne (board, path) = do
  start <- maybeToEither "failed to find start" $ findStart board
  let cursor = Cursor {direction = DirRight, pos = start}
  let end = foldl (executeInstruction board) cursor path
  Right $ score end

type CubeFace = M.Map (Int, Int) Tile

data Cube = Cube
  { faces :: M.Map (Int, Int) CubeFace,
    connections :: Connections,
    board :: Board,
    faceSize :: Int
  }

type Connections = M.Map ((Int, Int), Direction) ((Int, Int), Direction)

buildCube :: Connections -> Board -> Cube
buildCube connections board = Cube {connections, faces, board, faceSize}
  where
    keys = fst <$> M.toList board
    xs = fst <$> keys
    ys = snd <$> keys
    totalWidth = maximumOrZero xs - minimumOrZero xs + 1
    totalHeight = maximumOrZero ys - minimumOrZero ys + 1
    faceSize = gcd totalWidth totalHeight
    width = totalWidth `div` faceSize
    height = totalHeight `div` faceSize
    faces = M.fromList $ concat $ do
      [0 .. (width - 1)] <&> \faceX -> catMaybes $ do
        [0 .. (height - 1)] <&> \faceY -> do
          ((faceX, faceY),) . M.fromList . concat <$> do
            for [0 .. (faceSize - 1)] $ \fx -> do
              for [0 .. (faceSize - 1)] $ \fy -> do
                let x = faceX * faceSize + fx
                let y = faceY * faceSize + fy
                tile <- M.lookup (x, y) board
                let rx = x `mod` faceSize
                let ry = y `mod` faceSize
                pure ((rx, ry), tile)

connectionsTestInput :: Connections
connectionsTestInput =
  M.fromList
    [ (((2, 0), DirUp), ((0, 1), DirDown)),
      (((2, 0), DirRight), ((3, 2), DirLeft)),
      (((2, 0), DirDown), ((2, 1), DirDown)),
      (((2, 0), DirLeft), ((1, 1), DirDown)),
      (((0, 1), DirUp), ((2, 0), DirDown)),
      (((0, 1), DirRight), ((1, 1), DirRight)),
      (((0, 1), DirDown), ((2, 2), DirUp)),
      (((0, 1), DirLeft), ((3, 2), DirUp)),
      (((1, 1), DirUp), ((2, 0), DirRight)),
      (((1, 1), DirRight), ((2, 1), DirRight)),
      (((1, 1), DirDown), ((2, 2), DirRight)),
      (((1, 1), DirLeft), ((0, 1), DirLeft)),
      (((2, 1), DirUp), ((2, 0), DirUp)),
      (((2, 1), DirRight), ((3, 2), DirDown)),
      (((2, 1), DirDown), ((2, 2), DirDown)),
      (((2, 1), DirLeft), ((1, 1), DirLeft)),
      (((2, 2), DirUp), ((2, 1), DirUp)),
      (((2, 2), DirRight), ((3, 2), DirRight)),
      (((2, 2), DirDown), ((0, 1), DirUp)),
      (((2, 2), DirLeft), ((1, 1), DirUp)),
      (((3, 2), DirUp), ((2, 1), DirLeft)),
      (((3, 2), DirRight), ((2, 0), DirLeft)),
      (((3, 2), DirDown), ((0, 1), DirRight)),
      (((3, 2), DirLeft), ((2, 2), DirLeft))
    ]

connectionsRealInput :: Connections
connectionsRealInput =
  M.fromList
    [ (((1, 0), DirUp), ((0, 3), DirRight)),
      (((1, 0), DirRight), ((2, 0), DirRight)),
      (((1, 0), DirDown), ((1, 1), DirDown)),
      (((1, 0), DirLeft), ((0, 2), DirRight)),
      (((2, 0), DirUp), ((0, 3), DirUp)),
      (((2, 0), DirRight), ((1, 2), DirLeft)),
      (((2, 0), DirDown), ((1, 1), DirLeft)),
      (((2, 0), DirLeft), ((1, 0), DirLeft)),
      (((1, 1), DirUp), ((1, 0), DirUp)),
      (((1, 1), DirRight), ((2, 0), DirUp)),
      (((1, 1), DirDown), ((1, 2), DirDown)),
      (((1, 1), DirLeft), ((0, 2), DirDown)),
      (((0, 2), DirUp), ((1, 1), DirRight)),
      (((0, 2), DirRight), ((1, 2), DirRight)),
      (((0, 2), DirDown), ((0, 3), DirDown)),
      (((0, 2), DirLeft), ((1, 0), DirRight)),
      (((1, 2), DirUp), ((1, 1), DirUp)),
      (((1, 2), DirRight), ((2, 0), DirLeft)),
      (((1, 2), DirDown), ((0, 3), DirLeft)),
      (((1, 2), DirLeft), ((0, 2), DirLeft)),
      (((0, 3), DirUp), ((0, 2), DirUp)),
      (((0, 3), DirRight), ((1, 2), DirUp)),
      (((0, 3), DirDown), ((2, 0), DirDown)),
      (((0, 3), DirLeft), ((1, 0), DirDown))
    ]

data CubeCursor = CubeCursor {direction :: Direction, face :: (Int, Int), pos :: (Int, Int)}

scoreOnCube :: Cube -> CubeCursor -> Int
scoreOnCube (Cube {faceSize}) (CubeCursor {direction, pos = (x, y), face = (fx, fy)}) = do
  let column = x + fx * faceSize + 1
  let row = y + fy * faceSize + 1
  directionScore direction + 1000 * row + 4 * column

executeInstructionOnCube :: Cube -> CubeCursor -> Instruction -> CubeCursor
executeInstructionOnCube _ CubeCursor {direction, pos, face} TurnLeft = CubeCursor {direction = turnLeft direction, pos, face}
executeInstructionOnCube _ CubeCursor {direction, pos, face} TurnRight = CubeCursor {direction = turnRight direction, pos, face}
executeInstructionOnCube cube cursor (Move distance) = applyN distance (moveCursorOnCube cube) cursor

moveCursorOnCube :: Cube -> CubeCursor -> CubeCursor
moveCursorOnCube cube cursor@(CubeCursor {face, pos = (px, py), direction}) = fromMaybe cursor $ do
  faceMap <- M.lookup face cube.faces
  case M.lookup newPos faceMap of
    Nothing -> wrap cursor
    Just Air -> pure $ CubeCursor {pos = newPos, face, direction}
    Just Wall -> pure cursor
  where
    newX = px + directionX cursor.direction
    newY = py + directionY cursor.direction
    newPos = (newX, newY)

    wrap :: CubeCursor -> Maybe CubeCursor
    wrap cursor = do
      (face, direction) <- M.lookup (face, cursor.direction) cube.connections
      let maxFace = cube.faceSize - 1
      let (cursorX, cursorY) = cursor.pos
      let newPos = case (cursor.direction, direction) of
            (DirUp, DirDown) -> (maxFace - cursorX, cursorY)
            (DirRight, DirLeft) -> (cursorX, maxFace - cursorY)
            (DirDown, DirUp) -> (maxFace - cursorX, cursorY)
            (DirLeft, DirRight) -> (cursorX, maxFace - cursorY)
            (DirUp, DirUp) -> (cursorX, maxFace)
            (DirRight, DirRight) -> (0, cursorY)
            (DirDown, DirDown) -> (cursorX, 0)
            (DirLeft, DirLeft) -> (maxFace, cursorY)
            (DirUp, DirRight) -> (0, cursorX)
            (DirUp, DirLeft) -> (maxFace, maxFace - cursorX)
            (DirRight, DirDown) -> (maxFace - cursorY, 0)
            (DirRight, DirUp) -> (cursorY, maxFace)
            (DirDown, DirLeft) -> (maxFace, cursorX)
            (DirDown, DirRight) -> (0, maxFace - cursorX)
            (DirLeft, DirUp) -> (maxFace - cursorY, maxFace)
            (DirLeft, DirDown) -> (cursorY, 0)
      faceMap <- M.lookup face cube.faces
      case M.lookup newPos faceMap of
        Nothing -> Nothing
        Just Wall -> Nothing
        Just Air -> pure $ cursor {pos = newPos, direction = direction, face = face}

findStartOnCube :: Cube -> Maybe ((Int, Int), (Int, Int))
findStartOnCube (Cube {board, faceSize}) = do
  y <- safeMinimum $ map (snd . fst) $ M.toList board
  x <- safeMinimum $ map fst $ filter (snd >>> (== y)) $ map fst $ M.toList board
  let fx = x `div` faceSize
  let fy = y `div` faceSize
  let rx = x `mod` faceSize
  let ry = y `mod` faceSize
  pure ((fx, fy), (rx, ry))

solvePartTwo :: (Board, Path) -> Either String Int
solvePartTwo (board, path) = do
  let width = fromMaybe 0 $ safeMaximum $ map fst $ M.keys board
  let connections = if width > 100 then connectionsRealInput else connectionsTestInput
  let cube = buildCube connections board
  (face, start) <- maybeToEither "failed to find start" $ findStartOnCube cube
  let startCursor = (CubeCursor {direction = DirRight, pos = start, face})
  let end = foldl (executeInstructionOnCube cube) startCursor path
  Right $ scoreOnCube cube end

partOne :: String -> Either String String
partOne input = fmap show . solvePartOne =<< parse input

partTwo :: String -> Either String String
partTwo input = fmap show . solvePartTwo =<< parse input
