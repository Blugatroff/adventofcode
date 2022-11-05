module Util where

import Data.List (dropWhileEnd, isPrefixOf)
import qualified Data.Map as M

split :: Eq a => a -> [a] -> [[a]]
split del [] = []
split del list = f $ span (/= del) list
  where
    f ([], []) = []
    f ([], _ : rest) = [] : split del rest
    f (seg, []) = [seg]
    f (seg, _ : rest) = seg : split del rest

lpad :: Int -> a -> [a] -> [a]
lpad n v l = map (const v) [0 .. (n - length l)] ++ l

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace pat with [] = []
replace pat with rest =
  if pat `isPrefixOf` rest
    then with ++ drop (length pat) rest
    else head rest : replace pat with (tail rest)

trim :: (a -> Bool) -> [a] -> [a]
trim f = dropWhileEnd f . dropWhile f

findSeq :: Eq a => [a] -> [a] -> Maybe Int
findSeq [] list = error "empty pattern"
findSeq pat list = case span (\v -> v /= head pat) list of
  (before, []) -> Nothing
  (before, match) | pat `isPrefixOf` match -> Just $ length before
  (before, match) -> (+) (length before) <$> findSeq pat (tail match)

splitSeq :: Eq a => [a] -> [a] -> [[a]]
splitSeq del list = maybe [list] ((\(before, after) -> before : splitSeq del (drop (length del) after)) . flip splitAt list) (findSeq del list)

dedup :: Ord a => [a] -> [(a, Int)]
dedup list = M.toList $ foldl f M.empty list
  where
    f map k = M.insertWith (+) k 1 map

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (_ : list) = list
remove index (v : list) = v : remove (index - 1) list

set :: Int -> a -> [a] -> [a]
set index v = zipWith f [0 ..]
  where
    f i a = if i == index then v else a

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

decide :: a -> a -> Bool -> a
decide a b cond = if cond then a else b

chunks :: Int -> [a] -> [[a]]
chunks size [] = []
chunks size list = case splitAt size list of
  (chunk, rest) -> chunk : chunks size rest

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe (Right b) = Nothing

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left a) = Nothing
rightToMaybe (Right b) = Just b
