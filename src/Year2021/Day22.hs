module Year2021.Day22 (partOne, partTwo) where

import MeLude
import Data.Cuboid (Cuboid (..))
import Data.Cuboid qualified as Cuboid
import Data.Range (Range (..))
import Data.Range qualified as Range
import Util (readInteger, split, splitOnce, splitSeqOnce, trim)

data Change = On | Off deriving (Eq)

data Step = Step Change (Cuboid Integer)

parse :: String -> Either String [Step]
parse = traverse parseLine . filter (not . null) . map (trim isSpace) . lines

parseLine :: String -> Either String Step
parseLine line = maybeToEither ("failed to parse line: \"" <> line <> "\"") $ do
  (change, range) <- splitOnce ' ' line
  change <- case change of
    "on" -> Just On
    "off" -> Just Off
    _ -> Nothing
  cuboid <- parseCuboid range
  Just $ Step change cuboid

parseCuboid :: String -> Maybe (Cuboid Integer)
parseCuboid s =
  traverse parseRange (split ',' s) >>= \case
    [x, y, z] -> Just $ Cuboid x y z
    _ -> Nothing

parseRange :: String -> Maybe (Range Integer)
parseRange s = do
  (s, e) <- splitSeqOnce ".." s
  s <- s & filter (`elem` ("-0123456789" :: String)) & readInteger & eitherToMaybe
  e <- eitherToMaybe $ readInteger e
  Just $ Range.new s e

solvePartTwo :: [Step] -> Integer
solvePartTwo steps = sum $ map (\(Step ch cu) -> Cuboid.volume cu * direction ch) $ foldl go [] steps
  where
    flip On = Off
    flip Off = On

    direction On = 1
    direction Off = -1

    go :: [Step] -> Step -> [Step]
    go cuboids step = cuboids <> newCuboids
      where
        Step change cuboid = step
        intersections = cuboids & mapMaybe (\(Step ch cu) -> Step (flip ch) <$> Cuboid.intersection cuboid cu)

        newCuboids = case change of
          On -> Step change cuboid : intersections
          Off -> intersections

partOne ::String -> Either String String
partOne = Right . show . solvePartTwo . mapMaybe (\(Step ch cu) -> Step ch <$> Cuboid.intersection considered cu) <=< parse
  where
    consideredRange = Range (-50) 50
    considered = Cuboid consideredRange consideredRange consideredRange

partTwo ::String -> Either String String
partTwo = Right . show . solvePartTwo <=< parse

