module Year2024.Day5 (partOne, partTwo) where

import Data.Either.Extra (maybeToEither)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Util

type Rules = [(Int, Int)]

data Input = Input {rules :: Rules, updates :: [[Int]]}
  deriving (Show)

parse :: String -> Either String Input
parse input = do
  (rules, updates) <- maybeToEither "Failed to split input sections" $ do
    splitOnce [] $ lines input
  rules <- traverse parseRule rules
  updates <- traverse (traverse readInt . split ',') updates
  Right $ Input{rules, updates}
 where
  parseRule rule = do
    (before, after) <- maybeToEither ("Failed to parse rule: " <> rule) $ do
      splitOnce '|' rule
    (,) <$> readInt before <*> readInt after

withNeighbours :: [a] -> [([a], a, [a])]
withNeighbours = go []
 where
  go prev [] = []
  go prev (x : xs) = (prev, x, xs) : go (x : prev) xs

isCorrect :: Rules -> [Int] -> Bool
isCorrect rules = all pageIsCorrect . withNeighbours
 where
  pageIsCorrect (prev, page, next) = not $ any pageViolatesRule rules
   where
    pageViolatesRule (before, after) =
      (before == page && elem after prev)
        || (after == page && elem before next)

middleElement :: [a] -> Maybe a
middleElement [] = Nothing
middleElement xs = Just $ xs !! (length xs `div` 2)

partOne :: String -> Either String String
partOne input = do
  Input{rules, updates} <- parse input
  let correctUpdates = filter (isCorrect rules) updates
  Right $ show $ sum $ mapMaybe middleElement correctUpdates

correctUpdate :: Rules -> [Int] -> [Int]
correctUpdate rules update | isCorrect rules update = update
correctUpdate rules update = correctUpdate rules $ foldl (flip correctAccordingToRule) update rules

correctAccordingToRule :: (Int, Int) -> [Int] -> [Int]
correctAccordingToRule rule@(before, after) = forwards . reverse . backwards . reverse
 where
  forwards [] = []
  forwards [x] = [x]
  forwards (x : xs)
    | after == x && elem before xs =
        forwards (moveBehind x before xs)
  forwards (x : xs) = x : forwards xs

  backwards [] = []
  backwards [x] = [x]
  backwards (x : xs)
    | before == x && elem after xs =
        backwards (moveBehind x after xs)
  backwards (x : xs) = x : backwards xs

moveBehind :: (Eq a) => a -> a -> [a] -> [a]
moveBehind a behind [] = [a]
moveBehind a behind (x : xs) | x == behind && notElem behind xs = x : a : xs
moveBehind a behind (x : xs) = x : moveBehind a behind xs

partTwo :: String -> Either String String
partTwo input =
  parse input <&> \Input{rules, updates} ->
    filter (not . isCorrect rules) updates
      & mapMaybe (middleElement . correctUpdate rules)
      & sum
      & show

