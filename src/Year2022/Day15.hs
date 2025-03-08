module Year2022.Day15 (partOne, partTwo) where

import MeLude
import Util (TransparentString (..), readInt, split, splitOnce, trim)
import Data.Range qualified as Range
import Data.Range (Range(..))

type Pos = (Int, Int)

type Sensor = (Pos, Pos)

parseSensor :: String -> Either String Sensor
parseSensor line =
  case splitOnce ':' line of
    Nothing -> Left $ "failed to parse sensor " <> line
    Just (pos, beacon) -> do
      pos <- extractCoords pos
      beacon <- extractCoords beacon
      return (pos, beacon)
  where
    extractCoords input = case splitOnce ',' input of
      Nothing -> Left $ "failed to parse sensor " <> line
      Just (x, y) -> do
        x <- readInt (filter (\c -> isDigit c || c == '-') x)
        y <- readInt (filter (\c -> isDigit c || c == '-') y)
        return (x, y)

parse :: String -> Either String [Sensor]
parse input =
  split '\n' input
    <&> trim isSpace
    & filter (not . null)
    & traverse parseSensor

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

calculateScore :: Pos -> Int
calculateScore (x, y) = x * 4000000 + y

rangeFromSensor :: Int -> Sensor -> Maybe (Range Int)
rangeFromSensor y (sensor@(sensorX, sensorY), beacon) =
  if range < 0
    then Nothing
    else Just $ Range.new (sensorX - range) (sensorX + range)
  where
    range = manhattan beacon sensor - abs (sensorY - y)

rangesFromSensors :: Int -> [Sensor] -> [Range Int]
rangesFromSensors = mapMaybe . rangeFromSensor

solvePartOne :: Int -> [Sensor] -> Int
solvePartOne ySlice sensors = sum $ (\x -> x - 1) . Range.size <$> Range.tryMergeAll ranges
  where
    ranges = rangesFromSensors ySlice sensors

solvePartTwo :: Int -> [Sensor] -> TransparentString
solvePartTwo bounds sensors = TransparentString $ maybe "No solution found" (show . calculateScore) $ f 0
  where
    boundRange = Range 0 bounds

    f y | y > bounds = Nothing
    f y = case ranges y of
      [a, _] -> Just (Range.end a + 1, y)
      _ -> f (y + 1)

    ranges y = mapMaybe (Range.intersection boundRange) $ Range.tryMergeAll $ rangesFromSensors y sensors

isTestInput :: [Sensor] -> Bool
isTestInput = all ((< 1000000) . fst . fst)

part :: Show a => (Int -> [Sensor] -> a) -> Int -> Int -> String -> Either String String
part solve a b input =
  parse input
    <&> \sensors -> show $ solve (if isTestInput sensors then a else b) sensors

partOne :: String -> Either String String
partOne = part solvePartOne 10 2000000

partTwo :: String -> Either String String
partTwo = part solvePartTwo 20 4000000

