module Year2024.Day3 (partOne, partTwo) where

import MeLude
import Util

data Instr = Do | Dont | Mul Int Int

parse :: String -> [Instr]
parse [] = []
parse ('d' : 'o' : '(' : ')' : rest) = Do : parse rest
parse ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : rest) = Dont : parse rest
parse ('m' : 'u' : 'l' : '(' : rest) = fromMaybe (parse rest) $ do
  (a, rest) <- pure $ span isDigit rest
  when (null a || length a > 3) Nothing
  a <- eitherToMaybe $ readInt a
  rest <- case rest of ',' : rest -> Just rest; _ -> Nothing
  (b, rest) <- pure $ span isDigit rest
  b <- eitherToMaybe $ readInt b
  case rest of
    ')' : rest -> Just $ Mul a b : parse rest
    rest -> Nothing
parse (x : xs) = parse xs

partOne :: String -> Either String String
partOne =
  Right
    . show
    . sum
    . mapMaybe (\case Do -> Nothing; Dont -> Nothing; Mul a b -> Just (a * b))
    . parse

partTwo :: String -> Either String String
partTwo = Right . show . eval 1 . parse
 where
  eval :: Int -> [Instr] -> Int
  eval _ [] = 0
  eval s (x : xs) = case x of
    Do -> eval 1 xs
    Dont -> eval 0 xs
    Mul a b -> s * (a * b) + eval s xs
