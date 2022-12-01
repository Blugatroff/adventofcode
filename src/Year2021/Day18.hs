module Year2021.Day18 (partOne, partTwo) where

import Control.Monad.State (StateT (runStateT), evalStateT, get, lift, put)
import Data.Char (isDigit, isSpace)
import Data.Either.Combinators (mapLeft)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readEither)
import Util (maximum, maximumByKey, maybeToRight, sign, split, splitOnce, splitSeqOnce, trim)
import Prelude hiding (maximum)

data Element = Number !Int | Pair !Element !Element
  deriving (Show)

parseChar :: Char -> StateT String (Either String) Char
parseChar char =
  get >>= \case
    [] -> lift $ Left $ "expected " <> [char] <> " found empty"
    (c : rest)
      | c == char -> do
        put rest
        return c
      | otherwise -> lift $ Left $ "expected " <> [char] <> " found: " <> [c]

throwInState :: e -> StateT s (Either e) a
throwInState error = lift $ Left error

parseElement :: StateT String (Either String) Element
parseElement =
  get >>= \case
    [] -> throwInState "Expected element found empty"
    ('[' : rest) -> do
      put rest
      elem1 <- parseElement
      parseChar ','
      elem2 <- parseElement
      parseChar ']'
      return $ Pair elem1 elem2
    (first : rest)
      | isDigit first -> do
        put rest
        lift (Number <$> readEither [first])
      | otherwise -> throwInState $ "Expected element found: " <> (first : rest)

parse :: String -> Either String [Element]
parse input = traverse (evalStateT parseElement) (split '\n' input)

solvePartOne :: [Element] -> String
solvePartOne = show

solvePartTwo :: [Element] -> String
solvePartTwo = show

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
