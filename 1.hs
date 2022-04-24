import Util

solve :: [Int] -> Int
solve list = snd $ foldl f (head list, 0) $ tail list
    where
        f (p, c) v | v > p = (v, c + 1)
        f (p, c) v | otherwise = (v, c)

groups :: [Int] -> [[Int]]
groups (a:b:c:rest) = [a, b, c] : groups (b:c:rest)
groups _ = []

solve2 :: [Int] -> Int
solve2 = solve . map sum . groups

parse :: String -> [Int]
parse = map read . split '\n'

main :: IO ()
main = interact $ show . solve2 . parse

