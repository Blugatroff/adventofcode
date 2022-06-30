module Util where

import qualified Data.Array as A
import Data.Char (isSpace)
import Data.List (findIndex, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe

split :: Eq a => a -> [a] -> [[a]]
split del [] = []
split del list = f $ span (/= del) list
  where
    f ([], []) = []
    f ([], _ : rest) = [] : split del rest
    f (seg, []) = [seg]
    f (seg, _ : rest) = seg : split del rest

findSeq :: Eq a => [a] -> [a] -> Maybe Int
findSeq [] list = error "empty pattern"
findSeq pat list = case span (\v -> v /= head pat) list of
  (before, []) -> Nothing
  (before, match) | pat `isPrefixOf` match -> Just $ length before
  (before, match) -> (+) (length before) <$> findSeq pat (tail match)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : t) = t

splitSeq :: Eq a => [a] -> [a] -> [[a]]
splitSeq del list = maybe [list] ((\(before, after) -> before : splitSeq del (drop (length del) after)) . flip splitAt list) (findSeq del list)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace pat with [] = []
replace pat with rest =
  if pat `isPrefixOf` rest
    then with ++ drop (length pat) rest
    else head rest : replace pat with (tail rest)

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd f = reverse . dropWhile f . reverse

trim :: (a -> Bool) -> [a] -> [a]
trim f = dropWhileEnd f . dropWhile f

rpad :: Int -> a -> [a] -> [a]
rpad 0 def [] = []
rpad n def [] = def : rpad (n - 1) def []
rpad n def (a : r) = a : rpad (n - 1) def r

lpad :: Int -> a -> [a] -> [a]
lpad n v l = map (const v) [0 .. (n - length l)] ++ l

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

(|>) :: a -> (a -> b) -> b
a |> f = f a
