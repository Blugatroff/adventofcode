import Util

parse :: String -> [Int]
parse = map read . split ','

divRound :: Int -> Int -> Int
divRound a b = if m >= h then (a `div` b) + 1 else a `div` b
  where
    m = a `mod` b
    h = b `div` 2

average :: [Int] -> Int
average list = (`divRound` length list) $ sum list

distance :: Int -> Int -> Int
distance a = abs . (-) a

moveCost :: Int -> Int -> Int
moveCost a b = sum [0 .. distance a b]

fuelCost :: Int -> [Int] -> Int
fuelCost pos = foldl (\a n -> a + moveCost pos n) 0

costs :: [Int] -> [Int]
costs list = map (`fuelCost` list) [0 ..]

lastBeforeUp :: [Int] -> Int
lastBeforeUp (a : b : l) | a >= b = lastBeforeUp (b : l)
lastBeforeUp (a : b : l) = a
lastBeforeUp [a] = a
lastBeforeUp [] = error "lastBeforeUp of []"

solve :: [Int] -> Int
solve = lastBeforeUp . costs

main = interact $ show . solve . parse
