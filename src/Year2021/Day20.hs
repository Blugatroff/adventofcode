module Year2021.Day20 (partOne, partTwo) where

import Data.Bits (shiftL, (.|.))
import Data.Char (isSpace)
import Data.Either.Extra (maybeToEither)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Extra ((!?))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Pos (Pos (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import Util (applyN, forWithIndex, maximumOrZero, minimumOrZero, safeHead, safeTail, trace, trim)

data Input = Input {enhancement :: [Bool], image :: Set Pos}

parse :: String -> Either String Input
parse input = do
  let ls = fmap (trim isSpace) $ filter (not . null) $ lines input
  enhancement <- maybeToEither "enhancement algorithm missing" $ safeHead ls
  enhancement <- parseEnhancement enhancement
  image <- parseImage $ fromMaybe [] $ safeTail ls
  Right $ Input {enhancement, image}

parseEnhancement :: String -> Either String [Bool]
parseEnhancement = traverse parseTile

parseImage :: [String] -> Either String (Set Pos)
parseImage lines =
  Set.fromList . concat <$> do
    forWithIndex lines $ \y line ->
      catMaybes <$> do
        forWithIndex line $ \x char -> do
          tile <- parseTile char
          pure $
            if tile
              then Just $ Pos x y
              else Nothing

parseTile :: Char -> Either String Bool
parseTile '.' = Right False
parseTile '#' = Right True
parseTile c = Left $ show c <> " is not a tile"

imageDimensions :: Set Pos -> (Pos, Pos)
imageDimensions image = (Pos minX minY, Pos maxX maxY)
  where
    xs = x <$> Set.toList image
    ys = y <$> Set.toList image

    minX = minimumOrZero xs
    maxX = maximumOrZero xs
    minY = minimumOrZero ys
    maxY = maximumOrZero ys

readIntRadix2 :: [Bool] -> Int
readIntRadix2 = go . reverse
  where
    go [] = 0
    go (False : xs) = go xs `shiftL` 1
    go (True : xs) = go xs `shiftL` 1 .|. 1

enhance :: Bool -> [Bool] -> Set Pos -> Set Pos
enhance outOfBoundsState enhancement image = newImage
  where
    (Pos minX minY, Pos maxX maxY) = imageDimensions image

    neighbours (Pos x y) =
      [ Pos (x - 1) (y - 1),
        Pos x (y - 1),
        Pos (x + 1) (y - 1),
        Pos (x - 1) y,
        Pos x y,
        Pos (x + 1) y,
        Pos (x - 1) (y + 1),
        Pos x (y + 1),
        Pos (x + 1) (y + 1)
      ]

    outOfBounds (Pos x y) = x > maxX || x < minX || y > maxY || y < minY

    newImage = Set.fromList $ do
      [minX - 1 .. maxX + 1] >>= \x ->
        flip mapMaybe [minY - 1 .. maxY + 1] $ \y -> do
          let pos = Pos x y
          let isLit pos = if outOfBounds pos then outOfBoundsState else Set.member pos image
          let index = readIntRadix2 $ map isLit $ neighbours pos
          if fromMaybe False $ enhancement !? index then Just pos else Nothing

solve :: Int -> Input -> Int
solve n (Input {enhancement, image}) = Set.size $ applyN n (enhance True enhancement . enhance False enhancement) image

partOne :: String -> Either String String
partOne input = parse input <&> show . solve 1

partTwo :: String -> Either String String
partTwo input = parse input <&> show . solve 25
