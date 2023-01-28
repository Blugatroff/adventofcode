module Year2022.Day15 (partOne, partTwo) where

import Data.Char (isDigit, isSpace)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Util (TransparentString (..), readInt, split, splitOnce, trim)

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

data Range = Range Int Int

newRange :: Int -> Int -> Range
newRange s e | e >= s = Range s e
newRange s e = Range e s

rangeStart :: Range -> Int
rangeStart (Range s _) = s

rangeEnd :: Range -> Int
rangeEnd (Range _ e) = e

inRange :: Range -> Int -> Bool
inRange (Range s e) n = n >= s && n <= e

rangeSize :: Range -> Int
rangeSize (Range s e) = abs $ e - s

intersects :: Range -> Range -> Bool
intersects l@(Range sl el) r@(Range sr er) = inRange r sl || inRange r el || inRange l sr || inRange l er

tryMerge :: Range -> Range -> Either (Range, Range) Range
tryMerge l@(Range sl el) (Range sr er) | inRange l sr = Right $ Range sl (max el er)
tryMerge l@(Range sl el) (Range sr er) | inRange l er = Right $ Range (min sl sr) el
tryMerge (Range sl el) r@(Range sr er) | inRange r sl = Right $ Range sr (max el er)
tryMerge (Range sl el) r@(Range sr er) | inRange r el = Right $ Range (min sl sr) er
tryMerge (Range sl el) (Range sr er) | el + 1 == sr = Right $ Range sl er
tryMerge (Range sl el) (Range sr er) | er + 1 == sl = Right $ Range sr el
tryMerge l r = Left (l, r)

tryMergeAll :: [Range] -> [Range]
tryMergeAll ranges = f $ sortBy (compare `on` rangeStart) ranges
  where
    f :: [Range] -> [Range]
    f [] = []
    f [r] = [r]
    f (a : b : rest) = case tryMerge a b of
      Left (a, b) -> a : f (b : rest)
      Right r -> f $ r : rest

andRange :: Range -> Range -> Maybe Range
andRange a@(Range as ae) b@(Range bs be) | intersects a b = Just $ Range (max as bs) (min ae be)
andRange _ _ = Nothing

rangeFromSensor :: Int -> Sensor -> Maybe Range
rangeFromSensor y (sensor@(sensorX, sensorY), beacon) =
  if range < 0
    then Nothing
    else Just $ newRange (sensorX - range) (sensorX + range)
  where
    range = manhattan beacon sensor - abs (sensorY - y)

rangesFromSensors :: Int -> [Sensor] -> [Range]
rangesFromSensors = mapMaybe . rangeFromSensor

solvePartOne :: Int -> [Sensor] -> Int
solvePartOne ySlice sensors = sum $ rangeSize <$> tryMergeAll ranges
  where
    ranges = rangesFromSensors ySlice sensors

solvePartTwo :: Int -> [Sensor] -> TransparentString
solvePartTwo bounds sensors = TransparentString $ maybe "No solution found" (show . calculateScore) $ f 0
  where
    boundRange = Range 0 bounds

    f y | y > bounds = Nothing
    f y = case ranges y of
      [a, _] -> Just (rangeEnd a + 1, y)
      _ -> f (y + 1)

    ranges y = mapMaybe (andRange boundRange) $ tryMergeAll $ rangesFromSensors y sensors

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
