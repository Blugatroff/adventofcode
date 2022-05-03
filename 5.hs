import Data.List
import Util

type Point = (Int, Int)

type Line = (Point, Point)

intoTuple :: [a] -> (a, a)
intoTuple [a, b] = (a, b)
intoTuple b = error "expected 2 elements in list"

parsePoint :: String -> Point
parsePoint = intoTuple . map read . split ','

parseLine :: String -> Line
parseLine = intoTuple . map parsePoint . splitSeq " -> "

parse :: String -> [Line]
parse = map parseLine . split '\n'

sign :: Int -> Int
sign n | n > 0 = 1
sign n | n < 0 = -1
sign n = 0

genLine :: Line -> [Point]
genLine ((x1, y1), (x2, y2)) | x1 == x2 && y1 == y2 = [(x1, y1)]
genLine ((x1, y1), (x2, y2)) = (x1, y1) : genLine ((x1 + sign (x2 - x1), y1 + sign (y2 - y1)), (x2, y2))

computeScore :: [Point] -> Int
computeScore points = length $ filter (\(_, c) -> c >= 2) $ dedup points

solve2 :: [Line] -> Int
solve2 = computeScore . concatMap genLine

main = interact $ show . solve2 . parse
