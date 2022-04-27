{-# LANGUAGE LambdaCase #-}

import Control.Arrow
import Control.Concurrent
import Data.List
import GHC.Base
import Util

parseLine :: String -> [Bool]
parseLine = map (== '1')

parse :: String -> [[Bool]]
parse = map parseLine . split '\n'

mostCommon :: [Bool] -> Bool
mostCommon list = case () of
  () | a == b -> True
  () | a < b -> False
  _ -> True
  where
    a = length (filter id list)
    b = length (filter not list)

fromBitsRev :: [Bool] -> Int
fromBitsRev [] = 0
fromBitsRev (True : rest) = 1 + fromBitsRev (False : rest)
fromBitsRev (False : rest) = 2 * fromBitsRev rest

fromBits :: [Bool] -> Int
fromBits = reverse >>> fromBitsRev

solve :: [[Bool]] -> Int
solve lines = fromBits bits * fromBits (map not bits)
  where
    tl = transpose lines
    bits = map mostCommon tl

oxyBitCriteria :: [(Bool, a)] -> [a]
oxyBitCriteria column = map snd $ filter ((== mc) . fst) column
  where
    mc = mostCommon $ map fst column

scrubberBitCriteria :: [(Bool, a)] -> [a]
scrubberBitCriteria column = map snd $ filter ((/= mc) . fst) column
  where
    mc = mostCommon $ map fst column

ratingFromBitCriteria :: ([(Bool, [Bool])] -> [[Bool]]) -> [[Bool]] -> [Bool]
ratingFromBitCriteria criteria = f 0
  where
    f :: Int -> [[Bool]] -> [Bool]
    f cursor [n] = n
    f cursor numbers = f (cursor + 1) $ criteria $ map (\n -> (n !! cursor, n)) numbers

oxyRating :: [[Bool]] -> Int
oxyRating = fromBits . ratingFromBitCriteria oxyBitCriteria

scrubberRating :: [[Bool]] -> Int
scrubberRating = fromBits . ratingFromBitCriteria scrubberBitCriteria

solve2 :: [[Bool]] -> Int
solve2 numbers = oxyRating numbers * scrubberRating numbers

main :: IO ()
main = do
  numbers <- parse <$> getContents
  print $ solve numbers
  print $ solve2 numbers
  return ()
