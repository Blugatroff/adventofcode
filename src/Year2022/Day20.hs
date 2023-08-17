module Year2022.Day20 (partOne, partTwo) where

import Data.Char (isSpace)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Util (applyN, indexed, readInteger, split, trim)

parse :: String -> Either String [Integer]
parse = traverse readInteger . filter (not . null) . map (trim isSpace) . split '\n'

type Slot = (Int, Integer)

mix :: Seq Slot -> Slot -> Seq Slot
mix slots slot@(index, value) = do
  let index = Seq.findIndexL (== slot) slots
  case index of
    Nothing -> slots
    Just index -> do
      let size = toInteger $ Seq.length slots
      let newIndex = (toInteger index + value) `mod` (size - 1)
      let newInBounds = fromInteger $ if newIndex < 0 then newIndex + size - 1 else newIndex
      Seq.deleteAt index slots & Seq.insertAt newInBounds slot

groveCoords :: Seq (Int, Integer) -> [Integer]
groveCoords slots = fromMaybe [] $ do
  indexOfTheZero <- Seq.findIndexL ((== 0) . snd) slots
  let locations = [1000, 2000, 3000]
  let size = Seq.length slots
  let indexes = map ((`mod` size) . (+ indexOfTheZero)) locations
  map snd <$> traverse (`Seq.lookup` slots) indexes

decryptionKey :: Integer
decryptionKey = 811589153

solvePartOne :: [Integer] -> Integer
solvePartOne input = do
  let slots = Seq.fromList (indexed input)
  sum $ groveCoords $ foldl mix slots slots

solvePartTwo :: [Integer] -> Integer
solvePartTwo input = do
  let slots = Seq.fromList $ indexed (map (* decryptionKey) input)
  sum $ groveCoords $ applyN 10 (\init -> foldl mix init slots) slots

partOne :: String -> Either String String
partOne = fmap (show . solvePartOne) <$> parse

partTwo :: String -> Either String String
partTwo = fmap (show . solvePartTwo) <$> parse

