module Year2021.Day17 (partOne, partTwo) where

import MeLude
import Util (safeMaximum, maximumByKey, maybeToRight, sign, splitOnce, splitSeqOnce, trim)
import Data.Pos (Pos(..))

data Target = Target {minX :: !Int, maxX :: !Int, minY :: !Int, maxY :: !Int}
  deriving (Show)

parseRange :: String -> Either String (Int, Int)
parseRange range = case splitSeqOnce ".." (drop 1 $ dropWhile (/= '=') range) of
  Nothing -> Left $ "failed to parse range: " <> range
  Just (min, max) -> do
    min <- readEither min & first (const $ "failed to parse min: " <> min <> " of range: " <> range)
    max <- readEither max & first (const $ "failed to parse max: " <> max <> " of range: " <> range)
    return (min, max)

parse :: String -> Either String Target
parse input = do
  ranges <- splitOnce ':' input <&> trim isSpace . snd & maybeToRight ("failed to parse line: " <> input)
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

type Velocity = Pos

drag :: Int -> Int
drag n = n - sign n

step :: (Pos, Velocity) -> (Pos, Velocity)
step (Pos x y, Pos vx vy) = (Pos (x + vx) (y + vy), Pos (drag vx) (vy - 1))

isInTarget :: Target -> Pos -> Bool
isInTarget target (Pos x y) = x >= minX target && x <= maxX target && y >= minY target && y <= maxY target

targetReachable :: Target -> (Pos, Velocity) -> Bool
targetReachable target (Pos x y, Pos vx vy) = not $ vy < 0 && y < minY target

type Shot = [(Pos, Velocity)]

shoot :: Target -> (Pos, Velocity) -> Maybe Shot
shoot target (point, velocity)
  | not $ targetReachable target (point, velocity) = Nothing
  | isInTarget target point = Just [(point, velocity)]
  | otherwise = step (point, velocity) & shoot target <&> ((point, velocity) :)

shotScore :: Shot -> Int
shotScore steps = steps <&> y . fst & safeMaximum & fromMaybe 0

findBestShot :: [Shot] -> Maybe (Int, Shot)
findBestShot shots = shots <&> (\shot -> (shotScore shot, shot)) & maximumByKey fst

scanRange :: Target -> [Shot]
scanRange target = catMaybes $ xrange >>= \vx -> [minY target .. 2000] <&> \vy -> shoot target (Pos 0 0, Pos vx vy)
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
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input

