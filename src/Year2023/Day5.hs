module Year2023.Day5 (partOne, partTwo) where

import Data.List (foldl')

import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Util

data Mapping = Mapping {dst :: Int, src :: Int, len :: Int} deriving (Show)

type Map = [Mapping]

data Input = Input {seeds :: [Int], maps :: [Map]} deriving (Show)

parse :: String -> Either String Input
parse input = do
  (seeds, lines) <- case lines input of
    [] -> Left "Failed to parse seeds"
    seeds : maps -> Right (seeds, maps)
  seeds <- traverse readInt $ drop 1 $ split ' ' seeds
  maps <- parseMaps lines
  Right $ Input{seeds, maps}

parseMaps :: [String] -> Either String [Map]
parseMaps = traverse parseMap . filter (not . all (null . trimSpace)) . split []

parseMap :: [String] -> Either String Map
parseMap lines = traverse parseMapping $ drop 1 lines

parseMapping :: String -> Either String Mapping
parseMapping line =
  traverse readInt (split ' ' line) >>= \case
    [dst, src, length] -> Right $ Mapping dst src length
    _ -> Left $ "failed to parse mapping: " <> line

useMap :: Int -> Map -> Int
useMap v m = fromMaybe v $ flip findMap m $ \m ->
  if m.src <= v && v - m.src < m.len
    then Just (v + (m.dst - m.src))
    else Nothing

useMapReverse :: Map -> Int -> Int
useMapReverse m v = fromMaybe v $ flip findMap m $ \m ->
  if m.dst <= v && v - m.dst < m.len
    then Just (v + (m.src - m.dst))
    else Nothing

lowestSeedLocation :: [Map] -> [Int] -> Int
lowestSeedLocation maps = minimum . map (\seed -> foldl' useMap seed maps)

partOne :: String -> Either String String
partOne input = do
  Input{seeds, maps} <- parse input
  Right $ show $ lowestSeedLocation maps seeds

windows :: [a] -> [(a, a)]
windows [] = []
windows [x] = []
windows (x : y : xs) = (x, y) : windows xs

determinePointsOfInterest :: Input -> [Int]
determinePointsOfInterest Input{seeds, maps} = do
  let f poi m =
        fold
          [ poi
          , map (useMapReverse m) poi
          , m >>= \Mapping{src, len} -> [src, src + (len - 1)]
          ]
  let pointIsWithinAnySeedRange p = any (\(start, len) -> p >= start && p < start + len) $ windows seeds
  filter pointIsWithinAnySeedRange $ foldl' f [] (reverse maps)

partTwo :: String -> Either String String
partTwo input = do
  input <- parse input
  Right $ show $ lowestSeedLocation input.maps $ determinePointsOfInterest input
