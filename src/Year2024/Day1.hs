module Year2024.Day1 (partOne, partTwo) where

import MeLude
import Util

parse :: String -> Either String [(Int, Int)]
parse = traverse parseLine . filter (not . null) . map trimSpace . lines

parseLine :: String -> Either String (Int, Int)
parseLine line = do
  case filter (not . null) $ split ' ' line of
    [l, r] -> (,) <$> readInt l <*> readInt r
    _ -> Left $ "Failed to parse " <> line

partOne :: String -> Either String String
partOne input = do
  inputs <- parse input
  let lefts = sort (fst <$> inputs)
  let rights = sort (snd <$> inputs)
  Right $ show $ sum $ zipWith (fmap abs . (-)) lefts rights

partTwo :: String -> Either String String
partTwo input = do
  inputs <- parse input
  let lefts = fst <$> inputs
  let rights = snd <$> inputs
  let result = sum $ map (\id -> id * length (filter (== id) rights)) lefts
  Right $ show result

