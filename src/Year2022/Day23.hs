module Year2022.Day23 (partOne, partTwo) where

import Control.Arrow ((>>>))
import Data.Char (isSpace)
import Data.List.Extra (firstJust)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Traversable (for)
import Util (applyN, dedup, safeMaximum, safeMinimum, splitSeq, trim)
import Data.Pos (Pos(..))

type Board = S.Set Pos

parse :: String -> Either String Board
parse input =
  S.fromList . catMaybes . concat <$> do
    for (zip [0 ..] $ filter (not . null) $ map (trim isSpace) $ splitSeq "\n" input) $ \(y, line) -> do
      for (zip [0 ..] line) $ \(x, n) -> case n of
        '.' -> Right Nothing
        '#' -> Right $ Just $ Pos x y
        c -> Left $ "unknown tile: '" <> show c <> "'"

rotateArray :: [a] -> [a]
rotateArray a = drop 1 a <> take 1 a

rotateArrayN :: Int -> [a] -> [a]
rotateArrayN n a | n == length a = a
rotateArrayN n a | n > length a = rotateArrayN (n `mod` length a) a
rotateArrayN n a = applyN n rotateArray a

countMatching :: forall f a. Foldable f => (a -> Bool) -> f a -> Int
countMatching condition = foldl (\n a -> if condition a then n + 1 else n) 0

type Modification = (Pos, Pos)

runRound :: Int -> Board -> Board
runRound roundNumber board = foldl applyInsertion (foldl applyRemoval board freeModifications) freeModifications
  where
    elves = S.toList board

    applyRemoval board = fst >>> flip S.delete board
    applyInsertion board = snd >>> flip S.insert board

    freeModifications :: [Modification]
    freeModifications = filter f modifications
      where
        insertions = M.fromList (dedup (map snd modifications))
        f (_, dst) = M.lookup dst insertions == Just 1

    modifications :: [Modification]
    modifications = do
      let props = proposals board
      flip mapMaybe elves $ \elve -> do
        props <- props elve
        firstJust id $ rotateArrayN roundNumber props

    proposals :: Board -> Pos -> Maybe [Maybe Modification]
    proposals board elve@(Pos x y) = do
      let northWest = Pos (x - 1) (y - 1)
      let north = Pos x (y - 1)
      let northEast = Pos (x + 1) (y - 1)
      let west = Pos (x - 1) y
      let east = Pos (x + 1) y
      let southWest = Pos (x - 1) (y + 1)
      let south = Pos x (y + 1)
      let southEast = Pos (x + 1) (y + 1)
      let lookupOrEmpty = flip S.member board
      let nw = lookupOrEmpty northWest
      let no = lookupOrEmpty north
      let ne = lookupOrEmpty northEast
      let we = lookupOrEmpty west
      let ea = lookupOrEmpty east
      let sw = lookupOrEmpty southWest
      let so = lookupOrEmpty south
      let se = lookupOrEmpty southEast
      if not (nw || no || ne || we || ea || sw || so || se) then Nothing else Just ()
      Just
        [ case (nw, no, ne, we, ea, sw, so, se) of
            (False, False, False, _, _, _, _, _) -> Just (elve, north)
            (_, _, _, _, _, _, _, _) -> Nothing,
          case (nw, no, ne, we, ea, sw, so, se) of
            (_, _, _, _, _, False, False, False) -> Just (elve, south)
            (_, _, _, _, _, _, _, _) -> Nothing,
          case (nw, no, ne, we, ea, sw, so, se) of
            (False, _, _, False, _, False, _, _) -> Just (elve, west)
            (_, _, _, _, _, _, _, _) -> Nothing,
          case (nw, no, ne, we, ea, sw, so, se) of
            (_, _, False, _, False, _, _, False) -> Just (elve, east)
            (_, _, _, _, _, _, _, _) -> Nothing
        ]

countEmpty :: Board -> Int
countEmpty board = countMatching not $ do
  y <- [minY .. maxY]
  x <- [minX .. maxX]
  pure $ S.member (Pos x y) board
  where
    positions = S.toList board
    xs = map x positions
    ys = map y positions
    minX = fromMaybe 0 $ safeMinimum xs
    minY = fromMaybe 0 $ safeMinimum ys
    maxX = fromMaybe 0 $ safeMaximum xs
    maxY = fromMaybe 0 $ safeMaximum ys

solvePartOne :: Int -> Board -> Int
solvePartOne rounds board = countEmpty $ foldl (flip runRound) board [0 .. rounds - 1]

solvePartTwo :: Board -> Int
solvePartTwo = (+ 1) . f 0
  where
    f n board = if board == newBoard then n else f (n + 1) newBoard
      where
        newBoard = runRound n board

partOne :: String -> Either String String
partOne = fmap (show . solvePartOne 10) <$> parse

partTwo :: String -> Either String String
partTwo = fmap (show . solvePartTwo) <$> parse
