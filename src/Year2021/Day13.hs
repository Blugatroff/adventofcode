module Year2021.Day13 (partOne, partTwo) where

import MeLude
import Util (leftToMaybe, rightToMaybe, split)

data Fold = FoldAlongX !Int | FoldAlongY !Int
  deriving (Show)

data Input = Input
  { dots :: ![(Int, Int)],
    folds :: ![Fold]
  }
  deriving (Show)

type PaperSize = (Int, Int)

type Dots = [(Int, Int)]

parseFold :: String -> Either String Fold
parseFold line = value <&> axis
  where
    axis = if 'x' `elem` line then FoldAlongX else FoldAlongY
    value = readEither $ head $ tail $ split '=' line

parseDot :: String -> Either String (Int, Int)
parseDot line = case split ',' line <&> readEither of
  [x, y] -> do
    px <- x
    py <- y
    return (px, py)
  splits -> Left $ "failed to parse line: " <> line

parseLine :: String -> Either String (Either Fold (Int, Int))
parseLine line | "fold" `isPrefixOf` line = parseFold line <&> Left
parseLine line = parseDot line <&> Right

parse :: String -> Either String Input
parse input = do
  lines <- split '\n' input & filter (not . null) & traverse parseLine
  let dots = lines & mapMaybe rightToMaybe
  let folds = lines & mapMaybe leftToMaybe
  Right $ Input dots folds

foldAlongX :: Int -> Dots -> Dots
foldAlongX pos = map move
  where
    move :: (Int, Int) -> (Int, Int)
    move (x, y) | x <= pos = (x, y)
    move (x, y) = (pos - (x - pos), y)

foldAlongY :: Int -> Dots -> Dots
foldAlongY pos = map move
  where
    move :: (Int, Int) -> (Int, Int)
    move (x, y) | y <= pos = (x, y)
    move (x, y) = (x, pos - (y - pos))

foldPaper :: Dots -> Fold -> Dots
foldPaper dots = \case
  FoldAlongX x -> foldAlongX x dots
  FoldAlongY y -> foldAlongY y dots

solvePartOne :: Input -> Int
solvePartOne input = folds input & head & foldPaper (dots input) & nub & length

solvePartTwo :: Input -> String
solvePartTwo input = foldl' foldPaper (dots input) (folds input) & nub & showPaper

showPaper :: Dots -> String
showPaper dots =
  concatMap (<> "\n") $ do
    [0 .. height] <&> \y ->
        [0 .. width] <&> \x ->
          if (x, y) `elem` dots then '#' else '.'
  where
    (width, height) = measurePaper dots

measurePaper :: Dots -> PaperSize
measurePaper dots = (maximum $ map fst dots, maximum $ map snd dots)

partOne :: String -> Either String String
partOne input = parse input <&> (solvePartOne >>> show)

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo
