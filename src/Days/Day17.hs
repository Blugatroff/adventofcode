module Days.Day17 (partOne, partTwo) where

import Data.Char (isSpace)
import Data.Either.Combinators (mapLeft)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Text.Read (readEither)
import Util (maximumByKey, maybeToRight, sign, splitOnce, splitSeqOnce, trim)

data Target = Target {minX :: !Int, maxX :: !Int, minY :: !Int, maxY :: !Int}
  deriving (Show)

parseRange :: String -> Either String (Int, Int)
parseRange range = case splitSeqOnce ".." (drop 1 $ dropWhile (/= '=') range) of
  Nothing -> Left $ "failed to parse range: " <> range
  Just (min, max) -> do
    min <- readEither min & mapLeft (const $ "failed to parse min: " <> min <> " of range: " <> range)
    max <- readEither max & mapLeft (const $ "failed to parse max: " <> max <> " of range: " <> range)
    return (min, max)

parse :: String -> Either String Target
parse input = do
  ranges <- splitOnce ':' input <&> snd <&> trim isSpace & maybeToRight ("failed to parse line: " <> input)
  (xrange, yrange) <- maybeToRight ("failed to parse line: " <> input) $ splitOnce ',' ranges
  (minX, maxX) <- parseRange xrange
  (minY, maxY) <- parseRange yrange
  return $
    Target
      { minX = minX,
        maxX = maxX,
        minY = minY,
        maxY = maxY
      }

type Point = (Int, Int)

type Velocity = (Int, Int)

drag :: Int -> Int
drag n = n - sign n

step :: (Point, Velocity) -> (Point, Velocity)
step ((x, y), (vx, vy)) = ((x + vx, y + vy), (drag vx, vy - 1))

isInTarget :: Target -> (Int, Int) -> Bool
isInTarget target (x, y) = x >= minX target && x <= maxX target && y >= minY target && y <= maxY target

targetReachable :: Target -> (Point, Velocity) -> Bool
targetReachable target ((x, y), (vx, vy)) = not $ vy < 0 && y < minY target

type Shot = [(Point, Velocity)]

shoot :: Target -> (Point, Velocity) -> Maybe Shot
shoot target (point, velocity)
  | not $ targetReachable target (point, velocity) = Nothing
  | isInTarget target point = Just [(point, velocity)]
  | otherwise = step (point, velocity) & shoot target <&> ((point, velocity) :)

shotScore :: Shot -> Int
shotScore steps = steps <&> fst <&> snd & maximum

findBestShot :: [Shot] -> Maybe (Int, Shot)
findBestShot shots = shots <&> (\shot -> (shotScore shot, shot)) & maximumByKey fst

scanRange :: Target -> [Shot]
scanRange target = catMaybes $ xrange >>= \vx -> [minY target .. 2000] <&> \vy -> shoot target ((0, 0), (vx, vy))
  where
    xrange
      | minX target > 0 = [0 .. maxX target]
      | maxX target < 0 = [0 .. abs $ minX target] <&> negate
      | otherwise = [minX target .. maxX target]

solvePartOne :: Target -> Int
solvePartOne target = maybe 0 fst $ findBestShot $ scanRange target

solvePartTwo :: Target -> Int
solvePartTwo = length <$> scanRange

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
