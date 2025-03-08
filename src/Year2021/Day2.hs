module Year2021.Day2 (partOne, partTwo) where

import MeLude
import Util (split)

data Command
  = Down !Int
  | Up !Int
  | Forward !Int

parseCommand :: String -> Either String Command
parseCommand = f . split ' '
  where
    f ["forward", n] = Forward <$> readEither n
    f ["down", n] = Down <$> readEither n
    f ["up", n] = Up <$> readEither n
    f cmd = Left $ "invalid command " <> unwords cmd

parse :: String -> Either String [Command]
parse = traverse parseCommand . split '\n'

solvePartOne :: [Command] -> Int
solvePartOne commands = uncurry (*) $ foldl' f (0, 0) commands
  where
    f (x, y) (Down n) = (x, y + n)
    f (x, y) (Up n) = (x, y - n)
    f (x, y) (Forward n) = (x + n, y)

solvePartTwo :: [Command] -> Int
solvePartTwo commands = x * y
  where
    (_, x, y) = foldl' f (0, 0, 0) commands
    f :: (Int, Int, Int) -> Command -> (Int, Int, Int)
    f (aim, x, y) (Down n) = (aim + n, x, y)
    f (aim, x, y) (Up n) = (aim - n, x, y)
    f (aim, x, y) (Forward n) = (aim, x + n, y + aim * n)

partOne :: String -> Either String String
partOne input = show . solvePartOne <$> parse input

partTwo :: String -> Either String String
partTwo input = show . solvePartTwo <$> parse input
