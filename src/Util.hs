{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Util where

import Data.Either.Combinators (mapLeft)
import Data.Function ((&))
import Data.List (dropWhileEnd, foldl', isPrefixOf, maximumBy, minimumBy)
import Data.Map qualified as M
import Debug.Trace qualified as Trace
import Text.Read (readEither)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Char (isSpace)
import Data.Traversable (for)

split :: Eq a => a -> [a] -> [[a]]
split del [] = []
split del list = f $ span (/= del) list
  where
    f ([], []) = []
    f ([], _ : rest) = [] : split del rest
    f (seg, []) = [seg]
    f (seg, _ : rest) = seg : split del rest

splitOnce :: Eq a => a -> [a] -> Maybe ([a], [a])
splitOnce del list = case split del list of
  [a, b] -> Just (a, b)
  _ -> Nothing

lpad :: Int -> a -> [a] -> [a]
lpad n v l = map (const v) [0 .. (n - length l)] ++ l

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace pat with [] = []
replace pat with rest =
  if pat `isPrefixOf` rest
    then with ++ drop (length pat) rest
    else head rest : replace pat with (tail rest)

trim :: (a -> Bool) -> [a] -> [a]
trim f = dropWhileEnd f . dropWhile f

trimSpace :: String -> String
trimSpace = trim isSpace

findSeq :: Eq a => [a] -> [a] -> Maybe Int
findSeq [] list = Just 0
findSeq pat list = case span (\v -> v /= head pat) list of
  (before, []) -> Nothing
  (before, match) | pat `isPrefixOf` match -> Just $ length before
  (before, match) -> (+ 1) . (+) (length before) <$> findSeq pat (tail match)

splitSeq :: Eq a => [a] -> [a] -> [[a]]
splitSeq del list =
  findSeq del list
    & maybe
      [list]
      ( \splitIndex ->
          splitAt splitIndex list
            & \(before, after) -> before : splitSeq del (drop (length del) after)
      )

splitSeqOnce :: Eq a => [a] -> [a] -> Maybe ([a], [a])
splitSeqOnce del list = case splitSeq del list of
  [a, b] -> Just (a, b)
  _ -> Nothing

dedup :: Ord a => [a] -> [(a, Int)]
dedup list = M.toList $ foldl f M.empty list
  where
    f map k = M.insertWith (+) k 1 map

removeList :: Int -> [a] -> [a]
removeList _ [] = []
removeList 0 (_ : list) = list
removeList index (v : list) = v : removeList (index - 1) list

setList :: Int -> a -> [a] -> [a]
setList index v = zipWith f [0 ..]
  where
    f i a = if i == index then v else a

modifyList :: Int -> (a -> a) -> [a] -> [a]
modifyList index f = zipWith (\i a -> if i == index then f a else a) [0 ..]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just $ tail xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [a] = Just a
safeLast (a : rest) = safeLast rest

decide :: a -> a -> Bool -> a
decide a b cond = if cond then a else b

chunks :: Int -> [a] -> [[a]]
chunks size [] = []
chunks size list = case splitAt size list of
  (chunk, rest) -> chunk : chunks size rest

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (a, b) = (f a, f b)

mapTuple :: (a1 -> a2) -> (b1 -> b2) -> (a1, b1) -> (a2, b2)
mapTuple fa fb (a, b) = (fa a, fb b)

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe (Right b) = Nothing

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left a) = Nothing
rightToMaybe (Right b) = Just b

maximumByKey :: Ord b => (a -> b) -> [a] -> Maybe a
maximumByKey key [] = Nothing
maximumByKey key list = Just $ maximumBy (\a b -> compare (key a) (key b)) list

minimumByKey :: Ord b => (a -> b) -> [a] -> Maybe a
minimumByKey key [] = Nothing
minimumByKey key list = Just $ minimumBy (\a b -> compare (key a) (key b)) list

safeMinimum :: Ord a => [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum list = Just $ minimum list

safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum list = Just $ maximum list

minimumOrZero :: Ord a => Num a => [a] -> a
minimumOrZero = fromMaybe 0 . safeMinimum

maximumOrZero :: Ord a => Num a => [a] -> a
maximumOrZero = fromMaybe 0 . safeMaximum

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight error Nothing = Left error
maybeToRight error (Just value) = Right value

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f v = v
applyN n f v = applyN (n - 1) f (f v)

listToTuple :: [a] -> Maybe (a, a)
listToTuple [a, b] = Just (a, b)
listToTuple _ = Nothing

third :: (a, b, c) -> c
third (a, b, c) = c

sign :: Int -> Int
sign n | n > 0 = 1
sign n | n < 0 = -1
sign n = 0

readWithErrorMessage :: Read a => (String -> String) -> String -> Either String a
readWithErrorMessage error input = readEither input & mapLeft (const $ error input)

readInt :: String -> Either String Int
readInt = readWithErrorMessage $ \input -> "expected an int but got: \"" <> input <> "\""

readInteger :: String -> Either String Integer
readInteger = readWithErrorMessage $ \input -> "expected an integer but got: \"" <> input <> "\""

trace :: Show a => String -> a -> a
trace label value = Trace.trace (label <> ": " <> show value) value

reduceR :: (a -> a -> a) -> [a] -> Maybe a
reduceR fold [] = Nothing
reduceR fold (first : rest) = Just $ foldr fold first rest

reduceL :: (a -> a -> a) -> [a] -> Maybe a
reduceL fold [] = Nothing
reduceL fold (first : rest) = Just $ foldl' fold first rest

dropEnd :: [a] -> [a]
dropEnd [] = []
dropEnd [x] = []
dropEnd (x : xs) = x : dropEnd xs

mapWithPrevious :: (b -> a -> b) -> b -> [a] -> [b]
mapWithPrevious f previous [] = []
mapWithPrevious f previous (x : xs) = v : mapWithPrevious f v xs
  where
    v = f previous x

class Unwrap f a where
  unwrap :: f a -> a

instance Unwrap Maybe a where
  unwrap (Just a) = a
  unwrap Nothing = error "tried to unwrap Nothing"

instance Show e => Unwrap (Either e) a where
  unwrap (Right a) = a
  unwrap (Left e) = error $ "tried to unwrap a Left " <> show e

newtype TransparentString = TransparentString String

instance Show TransparentString where
  show (TransparentString s) = s


indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

tuplePermutations :: [a] -> [(a, a)]
tuplePermutations items =
  indexed items >>= \(i, item1) ->
    let f (j, item2) = if i /= j && j <= i then Just (item1, item2) else Nothing
    in mapMaybe f $ indexed items

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = safeHead . mapMaybe f

forWithIndex :: Applicative m => [a] -> (Int -> a -> m b) -> m [b]
forWithIndex xs f = for (zip [0..] xs) (uncurry f)

