module Year2022.Day15 (partOne, partTwo) where

import Data.Char (isSpace, isDigit)
import Data.Function ((&))
import Data.Functor ((<&>))
import Util (readInt, split, trim, readInt, splitOnce, safeHead, safeMaximum)
import Data.Maybe (catMaybes)
import Control.Applicative ((<|>))

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
parse input = split '\n' input 
  <&> trim isSpace 
  & filter (not . null) 
  & traverse parseSensor

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = (abs (x2 - x1)) + (abs (y2 - y1))

detectedBy :: [Sensor] -> Pos -> [Sensor]
detectedBy sensors pos = sensors & filter (\(sensor, beacon) -> manhattan pos sensor <= manhattan sensor beacon)

sample :: [Sensor] -> Pos -> Bool
sample sensors pos = case detectedBy sensors pos of
  [] -> False
  beacons -> all (\(sensor, beacon) -> sensor /= pos && beacon /= pos) beacons

solvePartOne :: Int -> [Sensor] -> Int
solvePartOne ySlice sensors = length $ filter (sample sensors) $ points
  where
    points = [leftMostBeacon..rightMostBeacon] <&> (,ySlice)
    leftMostBeacon = sensors <&> fst . snd & minimum
    rightMostBeacon = sensors <&> fst . snd & maximum

calculateScore :: Pos -> Int
calculateScore (x, y) = x * 4000000 + y

solvePartTwo :: Int -> [Sensor] -> Maybe Int
solvePartTwo bounds sensors = calculateScore <$> (safeHead $ catMaybes mins)
  where
    mins = sensors <&> fst <&> flowToMin bounds sensors Nothing

flowToMin :: Int -> [Sensor] -> Maybe Int -> Pos -> Maybe Pos
flowToMin bounds sensors previous (x, y) | x < 0 || x > bounds = Nothing
flowToMin bounds sensors previous (x, y) | y < 0 || y > bounds = Nothing
flowToMin bounds sensors previous (x, y) = case sample (x, y) of
  Nothing -> Just (x, y)
  Just height -> case previous of
    Just previous 
      | previous > height -> next height >>= flowToMin bounds sensors (Just height)
      | otherwise -> Nothing
    Nothing -> next height >>= flowToMin bounds sensors (Just height)
  where
    sample (x, y) = detectedBy sensors (x, y) <&> metric (x, y) & safeMaximum

    metric (x, y) (pos, beacon) = manhattan pos beacon - manhattan (x, y) pos

    next previous = 
      possible previous (x + 1, y)
      <|> possible previous (x - 1, y)
      <|> possible previous (x, y + 1)
      <|> possible previous (x, y - 1)

    possible previous (x, y) = case sample (x, y) of
      Nothing -> Just (x, y)
      Just height
          | height < previous -> Just (x, y)
          | otherwise -> Nothing
    

isTestInput :: [Sensor] -> Bool
isTestInput = all ((< 1000000) . fst . fst)

partOne :: String -> Either String String
partOne input = parse input <&> \sensors ->
  if isTestInput sensors 
    then solvePartOne 10 sensors & show
    else solvePartOne 2000000 sensors & show

partTwo :: String -> Either String String
partTwo input = parse input <&> \sensors ->
  if isTestInput sensors 
    then solvePartTwo 20 sensors & show
    else solvePartTwo 4000000 sensors & show
