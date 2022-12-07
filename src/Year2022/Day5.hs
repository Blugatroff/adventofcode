module Year2022.Day5 (partOne, partTwo) where

import Control.Loop
import Control.Monad (foldM, forM_)
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (ST, runST)
import Data.Char (isSpace)
import Data.Function (on, (&))
import Data.Functor (($>), (<&>))
import Data.List (foldl', sortBy, transpose)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Vector.Growable as V
import Foreign (Storable)
import Util (readInt, replace, split, splitOnce, trim)
import qualified Util

transposeStacks :: [String] -> [String]
transposeStacks input =
  transpose input
    <&> reverse
    <&> filter (/= ']')
    <&> filter (/= '[')
    <&> trim isSpace
    & filter (not . null)

parseStacks :: [String] -> [Stack]
parseStacks lines = lines <&> drop 1 <&> reverse

parseMove :: String -> Either String Move
parseMove line =
  line
    & replace "move" ""
    & replace "from" ""
    & replace "to" ""
    & split ' '
    & filter (not . null)
    & traverse readInt
    >>= \case
      [stack, source, destination] -> Right (Move stack source destination)
      notThreeInts -> Left $ "Failed to parse move from line: " <> line

parseMoves :: [String] -> Either String [Move]
parseMoves lines = lines & filter (not . null) & traverse parseMove

data Move = Move
  { amount :: !Int,
    source :: !Int,
    destination :: !Int
  }
  deriving (Show)

type Stack = [Char]

parse :: String -> Either String (M.Map Int Stack, [Move])
parse input = do
  let lines :: [String] = split '\n' input
  case splitOnce "" lines of
    Nothing -> Left "failed to find separation between stacks and moves"
    Just (stacks, moves) -> do
      moves <- parseMoves moves
      let stackMap = M.fromList $ zip [1 ..] $ parseStacks $ transposeStacks stacks
      return (stackMap, moves)

applyMove :: ([Char] -> [Char]) -> M.Map Int Stack -> Move -> M.Map Int Stack
applyMove reverseOrNot stacks move = case (M.lookup (source move) stacks, M.lookup (destination move) stacks) of
  (Just srcStack, Just destStack) ->
    M.insert (destination move) (reverseOrNot (take (amount move) srcStack) ++ destStack) stacks
      & M.insert (source move) (drop (amount move) srcStack)
  _ -> stacks

onTop :: M.Map Int Stack -> String
onTop stacks = M.elems stacks >>= take 1

solve :: ([Char] -> [Char]) -> (M.Map Int Stack, [Move]) -> String
solve reverseOrNot (stacks, moves) = output ++ onTop outputStacks
  where
    (_, output, outputStacks) = foldl' folder (0, "", stacks) moves

    numberMoves = length moves

    folder :: (Int, String, M.Map Int [Char]) -> Move -> (Int, String, M.Map Int [Char])
    folder (i, output, stacks) move = (i + 1, output ++ message, applyMove reverseOrNot stacks move)
      where
        message = show i <> "/" <> show numberMoves <> "\n"

solvePartOne :: (M.Map Int Stack, [Move]) -> String
solvePartOne = solve reverse

type FastStacks state = V.GrowableVector state (V.GrowablePrimitiveVector state Char)

newFastStack :: Storable a => [a] -> ST state (V.GrowablePrimitiveVector state a)
newFastStack items = do
  vector <- V.new
  forM_ (reverse items) $ V.push vector
  return vector

createFastStacks :: M.Map Int Stack -> ST state (FastStacks state)
createFastStacks map = do
  stackVector <- V.new
  newFastStack [] >>= V.push stackVector
  forM_ stacks $ \stack ->
    stack >>= V.push stackVector
  return stackVector
  where
    stacks = M.assocs map & sortBy (compare `on` fst) <&> snd <&> newFastStack

copy :: Storable a => Int -> Int -> Int -> V.GrowablePrimitiveVector state a -> V.GrowablePrimitiveVector state a -> ST state ()
copy srcIndex dstIndex length src dst =
  forLoop 0 (< length) (+ 1) $ \i ->
    V.write dst (dstIndex + i) =<< V.read src (srcIndex + i)

applyMoveFast :: FastStacks state -> Move -> ST state ()
applyMoveFast stacks move = do
  srcStack <- V.read stacks (source move)
  dstStack <- V.read stacks (destination move)
  dstIndex <- V.length dstStack
  srcIndex <- V.length srcStack <&> \length -> length - amount move
  let minDstSize = dstIndex + amount move
  whileM_ (V.length dstStack <&> (< minDstSize)) $ do
    V.push dstStack '?'
  copy srcIndex dstIndex (amount move) srcStack dstStack
  whileM_ (V.length srcStack <&> (> srcIndex)) $ do
    V.pop srcStack

vectorToList :: V.GrowableVector state a -> ST state [a]
vectorToList vector = do
  length <- V.length vector
  reverse <$> foldM fold [] [0 .. length - 1]
  where
    fold list i = do
      elem <- V.read vector i
      return $ elem : list

onTopFast :: FastStacks state -> ST state String
onTopFast stacks = reverse <$> (vectorToList stacks >>= foldM folder "")
  where
    folder output stack = V.length stack >>= (V.read stack . subtract 1) <&> (: output)

solveFast :: FastStacks state -> [Move] -> ST state String
solveFast stacks moves = do
  (_, outputStacks) <- foldM folder (0, stacks) moves
  onTopFast outputStacks
  where
    numberMoves = length moves

    folder :: (Int, FastStacks state) -> Move -> ST state (Int, FastStacks state)
    folder (i, stacks) move = do
      applyMoveFast stacks move
      return (i + 1, stacks)

solvePartTwo :: (M.Map Int Stack, [Move]) -> String
solvePartTwo (stacks, moves) = runST $ do
  fastStacks <- createFastStacks stacks
  solveFast fastStacks moves

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo
