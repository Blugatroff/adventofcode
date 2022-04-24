module Util where

split :: Eq a => a -> [a] -> [[a]]
split del [] = []
split del list = f $ span (/= del) list
    where 
        f ([], rest) = [rest]
        f (seg, []) = [seg]
        f (seg, _:rest) = seg : split del rest


