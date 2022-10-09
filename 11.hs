import Data.Char (digitToInt, isControl, isSpace)
import Data.Foldable (find)
import Data.List (elemIndex, findIndex, sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Util (remove, set, split, trim, (|>), safeHead, (<#>), (>>>), indexed)

data Grid = Grid { width :: Int, height :: Int, cells :: [Int] }
    deriving (Show)

parse :: String -> Grid
parse input = let 
        rows :: [[Char]]
        rows = split '\n' input |> filter (not . null)
        width = length rows
        height = safeHead rows <#> length |> fromMaybe 0
        chars = concat rows 
        ints = chars <#> (read . (\c -> [c]))
    in Grid width height ints

incrementGrid :: Grid -> Grid
incrementGrid grid = Grid 
    (width grid) 
    (height grid) 
    (cells grid <#> \n -> n + 1)

gridFields :: Grid -> [(Int, Int, Int)]
gridFields (Grid width _ cells) = cells
    |> indexed
    |> map (\(i, v) -> (i `mod` width, i `div` width, v))

findFlashing :: Grid -> [(Int, Int)]
findFlashing = gridFields 
    >>> filter (\(_, _, value) -> value > 9) 
    >>> map (\(x, y, _) -> (x, y))

isAffected :: Int -> Int -> Int -> Int -> Bool
isAffected x1 y1 x2 y2 = (abs (x1 - x2) <= 1) && (abs (y1 - y2) <= 1)

applyFlash :: (Grid, Int) -> (Int, Int) -> (Grid, Int)
applyFlash (grid, flashCount) (x, y) = gridFields grid
    <#> (\(x2, y2, val) -> if x2 == x && y2 == y then 
            0 
        else if isAffected x y x2 y2 then 
            val + 1 
        else 
            val
        )
    |> Grid (width grid) (height grid)
    |> \grid -> applyFlashes (grid, flashCount + 1) (findFlashing grid)

applyFlashes :: (Grid, Int) -> [(Int, Int)] -> (Grid, Int)
applyFlashes = foldl applyFlash

step :: Grid -> (Grid, Int)
step = incrementGrid >>> (\grid -> applyFlashes (grid, 0) (findFlashing grid))

solve :: Int -> Grid -> Int
solve steps grid = snd $ foldl f (grid, 0) [1..steps]

f :: (Grid, Int) -> a -> (Grid, Int)
f (grid, count) _ = step grid |> (\(g, c) -> (g, c + count))

main = interact $ show . solve 2 . parse

