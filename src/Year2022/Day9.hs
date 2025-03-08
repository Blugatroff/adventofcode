module Year2022.Day9 (partOne, partTwo) where

import MeLude
import qualified Data.List.NonEmpty as NE
import Util (dedup, mapFst, mapSnd, mapWithPrevious, readInt, sign, split, splitOnce, trim)

data Instruction = U | D | L | R
  deriving (Show)

type Rope = NE.NonEmpty (Int, Int)

parseDirection :: String -> Either String Instruction
parseDirection "U" = Right U
parseDirection "D" = Right D
parseDirection "L" = Right L
parseDirection "R" = Right R
parseDirection direction = Left $ "failed to parse instruction: " <> direction

parseLine :: String -> Either String [Instruction]
parseLine s = do
  case trim isSpace s & splitOnce ' ' of
    Just (direction, count) -> do
      direction <- parseDirection direction
      count <- readInt count
      return $ replicate count direction
    Nothing -> Left $ "Failed to parse line: " <> s

parse :: String -> Either String [Instruction]
parse input = split '\n' input <&> trim isSpace & traverse parseLine <&> concat

pull :: (Int, Int) -> (Int, Int) -> (Int, Int)
pull (headX, headY) (tailX, tailY)
  | xDistance <= 1 && yDistance <= 1 = (tailX, tailY)
  | xDistance == 2 && yDistance == 0 = (tailX + xDirection, tailY)
  | xDistance == 0 && yDistance == 2 = (tailX, tailY + yDirection)
  | otherwise = (tailX + xDirection, tailY + yDirection)
  where
    xDistance = abs (headX - tailX)
    yDistance = abs (headY - tailY)
    xDirection = sign (headX - tailX)
    yDirection = sign (headY - tailY)

executeInstruction :: Rope -> Instruction -> Rope
executeInstruction rope instructions = pullRope $
  case instructions of
    U -> updateHead (mapSnd (subtract 1)) rope
    D -> updateHead (mapSnd (+ 1)) rope
    L -> updateHead (mapFst (subtract 1)) rope
    R -> updateHead (mapFst (+ 1)) rope
  where
    updateHead :: ((Int, Int) -> (Int, Int)) -> Rope -> Rope
    updateHead f (head :| body) = f head :| body

    pullRope :: Rope -> Rope
    pullRope (head :| body) = head :| mapWithPrevious pull head body

solve :: Int -> [Instruction] -> Int
solve tailLength instructions = foldl' fold ((0, 0) :| replicate tailLength (0, 0), []) instructions & snd & dedup & length
  where
    fold :: (Rope, [(Int, Int)]) -> Instruction -> (Rope, [(Int, Int)])
    fold (rope, path) instruction = (movedRope, visited : path)
      where
        movedRope = executeInstruction rope instruction
        visited = NE.last movedRope

partOne :: String -> Either String String
partOne input = parse input <&> solve 1 <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solve 9 <&> show
