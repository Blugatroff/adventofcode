module Year2022.Day14 (partOne, partTwo) where

import MeLude hiding (range)
import Util (readInt, split, trim, readInt, splitSeq, splitOnce)
import qualified Data.Map as M
import Control.Monad.State (State, gets, modify, evalState)

type Path = [(Int, Int)]

parsePath :: String -> Either String Path
parsePath line = do
  for (splitSeq "->" line) (trim isSpace >>> splitOnce ',' >>> \case
    Just (x, y) -> do
      x <- readInt x
      y <- readInt y
      return (x, y)
    Nothing -> Left $ "failed to parse path: " <> line)

parse :: String -> Either String [Path]
parse input = split '\n' input <&> trim isSpace & filter (not . null) & traverse parsePath

data Block = Stone | Sand
    deriving(Show, Eq)

type Cave = M.Map (Int, Int) Block

range :: Int -> Int -> [Int]
range s e | s < e = [s..e]
range s e = [e..s]

insertStone :: (Int, Int) -> (Int, Int) -> State Cave ()
insertStone (x1, y1) (x2, y2) | y1 == y2 = forM_ (range x1 x2) $ \x -> modify $ M.insert (x, y1) Stone
insertStone (x1, y1) (x2, y2) | x1 == x2 = forM_ (range y1 y2) $ \y -> modify $ M.insert (x1, y) Stone
insertStone _ _ = pure ()

insertPath :: Path -> State Cave ()
insertPath path = do
    forM_ (zip path (drop 1 path)) $ \(from, to) -> do
        insertStone from to

insertPaths :: [Path] -> State Cave ()
insertPaths paths = do
    forM_ paths insertPath

drawCave :: Cave -> String
drawCave cave = intercalate "\n" ([minY..maxY] <&> \y ->
        [minX..maxX] <&> (\x ->
            case M.lookup (x, y) cave of
                Nothing -> '.'
                Just Stone -> '#'
                Just Sand -> 'o'))
    where
        minX = M.keys cave <&> fst & minimum
        minY = M.keys cave <&> snd & minimum

        maxX = M.keys cave <&> fst & maximum
        maxY = M.keys cave <&> snd & maximum

simulateSand :: Int -> (Int, Int) -> State Cave Bool
simulateSand floor pos@(x, y) = do
    block <- gets $ M.lookup pos
    case block of
        Nothing -> do
            let downPos = (x, y + 1)
            let leftPos = (x - 1, y + 1)
            let rightPos = (x + 1, y + 1)
            down <- gets $ M.lookup downPos
            left <- gets $ M.lookup leftPos
            right <- gets $ M.lookup rightPos
            let pos = (case (down, left, right) of
                    (Nothing, _, _) -> Just downPos
                    (_, Nothing, _) -> Just leftPos
                    (_, _, Nothing) -> Just rightPos
                    (_, _, _) -> Nothing)
            case pos of
                Just pos -> do
                    if y + 1 < floor then do
                        simulateSand floor pos
                    else do
                        modify $ M.insert (x, y) Sand
                        pure True
                Nothing -> do
                    modify $ M.insert (x, y) Sand
                    pure False
        _ -> pure False

findAbyss :: Cave -> Int
findAbyss cave = M.assocs cave & filter ((==Stone) . snd) <&> snd . fst & maximum

solvePartOne :: [Path] -> String
solvePartOne paths = flip evalState M.empty $ do
    insertPaths paths
    abyss <- gets findAbyss
    count <- untilAbyss abyss
    gets $ drawCave >>> (<> "\n" <> show count)
    where
        untilAbyss :: Int -> State Cave Int
        untilAbyss abyss = do
            fellIntoAbyss <- simulateSand abyss (500, 0)
            if fellIntoAbyss
              then return 0 
              else untilAbyss abyss <&> (+1)

solvePartTwo :: [Path] -> Int
solvePartTwo paths = flip evalState M.empty $ do
    insertPaths paths
    floor <- gets $ (+2) . findAbyss
    untilFull floor
    where
        untilFull :: Int -> State Cave Int
        untilFull floor = do
            gets (M.lookup (500, 0)) >>= \case
                Just _ -> return 0
                Nothing -> do
                    _ <- simulateSand floor (500, 0)
                    untilFull floor <&> (+1)

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne

partTwo :: String -> Either String String
partTwo input = parse input <&> (solvePartTwo >>> show)

