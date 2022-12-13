module Year2021.Day8 (partOne, partTwo) where

import Data.Char (isSpace)
import Data.List (elemIndex, findIndex, sort)
import GHC.List (foldl')
import Util (removeList, setList, split, trim)

parseSegments :: String -> [String]
parseSegments = filter (not . null) . map (trim isSpace) <$> split ' '

parseLine :: String -> ([String], [String])
parseLine line = case map parseSegments $ split '|' line of
  [a, b] -> (a, b)
  notAPair -> error "failed to split once"

parse :: String -> [([String], [String])]
parse input = map parseLine $ filter (not . null) $ map (trim isSpace) $ split '\n' input

isSuperSet :: Eq a => [a] -> [a] -> Bool
isSuperSet a = all (`elem` a)

-- digits with 5 segments: 5 2 3
-- digits with 6 segments: 0 6

-- STEPS
-- - 1: has 2 segments
-- - 7: has 3 segments
-- - 4: has 4 segments
-- - 8: has 7 segments
-- - 3: has 5 segments and contains both segments of 1
-- - 6: has 6 segments and does not contain all segment of 1
-- - 0: has 6 segments and does not contain all segments of the number 4
-- - 9: has 6 segments
-- - 2: has 5 segments and is not a subset of 6
-- - 5: has 5 segments

type Step = (Int, String -> [String] -> Bool)

steps :: [Step]
steps =
  [ (1, \s _ -> length s == 2),
    (7, \s _ -> length s == 3),
    (4, \s _ -> length s == 4),
    (8, \s _ -> length s == 7),
    (3, \s results -> length s == 5 && isSuperSet s (results !! 1)),
    (6, \s results -> length s == 6 && not (isSuperSet s (results !! 1))),
    (0, \s results -> length s == 6 && not (isSuperSet s (results !! 4))),
    (9, \s _ -> length s == 6),
    (2, \s results -> length s == 5 && not (isSuperSet (results !! 6) s)),
    (5, \s _ -> length s == 5)
  ]

extract :: (a -> Bool) -> [a] -> ([a], a)
extract f list = case index of
  Just index -> (removeList index list, list !! index)
  Nothing -> (list, undefined)
  where
    index = findIndex f list

solveSegments :: [String] -> [String]
solveSegments segments = snd $ foldl' f (segments, replicate 10 "") steps
  where
    f :: ([String], [String]) -> Step -> ([String], [String])
    f (segments, results) (n, step) = (rem, setList n pat results)
      where
        (rem, pat) = extract (`step` results) segments

unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = error "tried to unwrap Nothing"

matchOutputs :: [String] -> [String] -> [Int]
matchOutputs segments = map unwrap <$> map ((`elemIndex` map sort segments) . sort)

digitsToInt :: [Int] -> Int
digitsToInt digits = f $ reverse digits
  where
    f [n] = n
    f (n : list) = n + (10 * f list)
    f [] = error "no digits"

solveLine :: ([String], [String]) -> Int
solveLine (segments, outputs) = digitsToInt $ matchOutputs (solveSegments segments) outputs

solvePartTwo :: [([String], [String])] -> Int
solvePartTwo = sum . map solveLine

solvePartOne :: [([String], [String])] -> Int
solvePartOne = sum . map (sum . map (isSimpleDigit . length) . snd)
  where
    isSimpleDigit :: Int -> Int
    isSimpleDigit 2 = 1
    isSimpleDigit 4 = 1
    isSimpleDigit 3 = 1
    isSimpleDigit 7 = 1
    isSimpleDigit _ = 0

partOne :: String -> Either String String
partOne = Right . show . solvePartOne . parse

partTwo :: String -> Either String String
partTwo = Right . show . solvePartTwo . parse
