module Util where

import Data.List (isPrefixOf)

split :: Eq a => a -> [a] -> [[a]]
split del [] = []
split del list = f $ span (/= del) list
  where
    f ([], []) = []
    f ([], _ : rest) = [] : split del rest
    f (seg, []) = [seg]
    f (seg, _ : rest) = seg : split del rest

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
lpad n v l = map (const v) [0..(n - length l)] ++ l
