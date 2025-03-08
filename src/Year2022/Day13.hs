module Year2022.Day13 (partOne, partTwo) where

import MeLude
import Util (chunks, readInt, split, trim)

data Element = Number !Int | List ![Element]
  deriving (Show, Eq)

instance Ord Element where
  compare (Number a) (Number b) = compare a b
  compare (List a) (List b) = compare a b
  compare a@(Number _) b@(List _) = compare (List [a]) b
  compare a@(List _) b@(Number _) = compare a (List [b])

parseElement :: String -> Either String (Element, String)
parseElement [] = Left "expected element got ''"
parseElement input@(c : rest) | isDigit c = do
  n <- readInt $ takeWhile isDigit input
  return $ (Number n, dropWhile isDigit input)
parseElement ('[' : ']' : rest) = Right (List [], rest)
parseElement ('[' : rest) = do
  (elems, rest) <- run [] rest
  case rest of
    ']' : rest -> Right (List elems, rest)
    rest -> Left $ "expected ] got " <> rest
  where
    run :: [Element] -> String -> Either String ([Element], String)
    run previous rest = do
      (el, rest) <- parseElement rest
      case rest of
        ',' : rest -> do
          run (previous <> [el]) rest
        rest -> Right (previous <> [el], rest)
parseElement input = Left $ "unexpected input " <> input

pairs :: [a] -> [(a, a)]
pairs list =
  chunks 2 list >>= \case
    [a, b] -> [(a, b)]
    _ -> []

parse :: String -> Either String [Element]
parse input = split '\n' input <&> trim isSpace & filter (not . null) & traverse (fmap fst . parseElement)

solvePartOne :: [Element] -> Int
solvePartOne elements =
  pairs elements
    <&> uncurry (<)
    & zip [1 ..]
    & filter snd
    <&> fst
    & sum

dividers :: [Element]
dividers = [List [List [Number 2]], List [List [Number 6]]]

solvePartTwo :: [Element] -> Int
solvePartTwo elements =
  dividers
    <&> (`elemIndex` sorted)
    & catMaybes
    <&> (+ 1)
    & product
  where
    sorted = sort (elements <> dividers)

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
