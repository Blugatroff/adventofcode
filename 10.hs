import Data.Char (digitToInt, isControl, isSpace)
import Data.Foldable (find)
import Data.List (elemIndex, findIndex, sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Util (remove, set, split, trim, (|>))

parse :: String -> [[Char]]
parse input =
  split '\n' input
    |> filter (not . null)

parens :: [(Char, Char)]
parens =
  [ ('(', ')'),
    ('[', ']'),
    ('{', '}'),
    ('<', '>')
  ]

data Error = ExpectedClosing Char (Maybe Char) | ExpectedOpeningParen
  deriving (Show)

isClosingParen :: Char -> Bool
isClosingParen c = not $ null $ find (\(open, close) -> close == c) parens

parenPoints :: Char -> Int
parenPoints ')' = 3
parenPoints ']' = 57
parenPoints '}' = 1197
parenPoints '>' = 25137
parenPoints _ = undefined

match :: [Char] -> Either Error String
match "" = Right ""
match line = matchOnce line >>= match
  where
    matchOnce "" = Right ""
    matchOnce [c] = case find (\(open, close) -> open == c) parens of
      Just (open, close) -> Left $ ExpectedClosing close Nothing
      Nothing -> Left ExpectedOpeningParen
    matchOnce (c1 : c2 : line) = case find (\(open, close) -> open == c1) parens of
      Just (open, close) | c2 == close -> Right line
      Just (open, close) -> f close (c2 : line)
      Nothing -> Left ExpectedOpeningParen

    f :: Char -> [Char] -> Either Error String
    f close [] = Left $ ExpectedClosing close Nothing
    f close (c : end) | c == close = Right end
    f close (c : end) | isClosingParen c = Left $ ExpectedClosing close $ Just c
    f close l = matchOnce l >>= f close

isIncomplete :: Either Error String -> Bool
isIncomplete res = case res of
  Left (ExpectedClosing _ Nothing) -> True
  _ -> False

flipParen :: Char -> Char
flipParen '(' = ')'
flipParen '<' = '>'
flipParen '{' = '}'
flipParen '[' = ']'
flipParen ')' = '('
flipParen '>' = '<'
flipParen '}' = '{'
flipParen ']' = '['
flipParen _ = undefined

complete :: [Char] -> [Char] -> [Char]
complete [] stack = stack
complete (c : end) stack | isClosingParen c = complete end $ tail stack
complete (c : end) stack = complete end (flipParen c : stack)

illegalChar :: Either Error String -> Maybe Char
illegalChar (Left (ExpectedClosing _ expected)) = expected
illegalChar _ = Nothing

solveLine :: [Char] -> Maybe Int
solveLine line = parenPoints <$> illegalChar (match line)

solve :: [[Char]] -> Int
solve lines = sum $ map (fromMaybe 0 . solveLine) lines

solveLine2 :: [Char] -> Maybe String
solveLine2 line | isIncomplete $ match line = Just (complete line "")
solveLine2 _ = Nothing

parenPoints2 :: Char -> Int
parenPoints2 ')' = 1
parenPoints2 ']' = 2
parenPoints2 '}' = 3
parenPoints2 '>' = 4
parenPoints2 _ = undefined

computeScore :: Int -> [Char] -> Int
computeScore = foldl (\score c -> score * 5 + parenPoints2 c)

solve2 :: [[Char]] -> Int
solve2 lines = scores !! (length scores `div` 2)
  where
    scores = sort $ mapMaybe (fmap (computeScore 0) . solveLine2) lines

main = interact $ show . solve2 . parse
