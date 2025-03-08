module Year2024.Day11 (partOne, partTwo) where

import MeLude
import Util

import Control.Monad.State.Strict as State
import Data.Map qualified as Map

parse :: String -> Either String [Int]
parse = traverse readInt . filter (not . null) . split ' '

blink :: Int -> [Int]
blink 0 = [1]
blink x = do
  let l = lengthInBase 10 x
  if even l
    then do
      let k = 10 ^ (l `div` 2)
      [x `div` k, x `mod` k]
    else do
      [x * 2024]

blinkN :: Int -> [Int] -> Int
blinkN n xs = flip evalState Map.empty $ do
  fmap sum $ for xs $ \x -> do
    let go :: Int -> Int -> State (Map (Int, Int) Int) Int
        go 0 x = pure 1
        go 1 x = do
          let n = length (blink x)
          modify $ Map.insert (1, x) n
          pure n
        go n x = do
          let xs = blink x
          res <- fmap sum $ for xs $ \x -> do
            gets (Map.lookup (n - 1, x)) >>= \case
              Just res -> pure res
              Nothing -> go (n - 1) x
          modify $ Map.insert (n, x) res
          pure res
    go n x

partOne :: String -> Either String String
partOne input = show . blinkN 25 <$> parse input

partTwo :: String -> Either String String
partTwo input = show . blinkN 75 <$> parse input
