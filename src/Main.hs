module Main where

import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Maybe
import qualified Days
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readEither)

data Part = PartOne | PartTwo
  deriving (Show)

newtype Day = Day Int
  deriving (Show)

data Args
  = Help
  | Args Day (Maybe Part)
  deriving (Show)

validateDay :: Int -> Either String Day
validateDay day
  | day - 1 >= length Days.days || day <= 0 =
    Left $ "day must be above 0 and below " <> show (length Days.days + 1)
validateDay day = Right $ Day $ day - 1

parseArgs :: [String] -> Either String Args
parseArgs [] = Right Help
parseArgs ["--help"] = Right Help
parseArgs [day] = readEither day >>= validateDay <&> flip Args Nothing
parseArgs [day, part] = do
  day <- readEither day >>= validateDay
  part <- case part of
    "one" -> Right PartOne
    "two" -> Right PartTwo
    "1" -> Right PartOne
    "2" -> Right PartTwo
    part -> Left $ "unexpected part '" <> part <> "' use either 'one', 'two', '1' or '2'"
  return $ Args day $ Just part
parseArgs _ = Left "unexpected arguments, use --help"

abort :: String -> IO a
abort error = hPutStrLn stderr error >> exitFailure

chooseSolveFunction :: Maybe Part -> Days.Day -> Days.Part
chooseSolveFunction part day = case fromMaybe PartOne part of
  PartOne -> Days.partOne day
  PartTwo -> Days.partTwo day

start :: Args -> IO ()
start Help = do
  putStrLn "Advent Of Code 2021 in Haskell"
  putStrLn ""
  putStrLn "Usage: ./aoc2021 <day> <part>"
start (Args (Day dayIndex) part) = do
  let day = Days.days !! dayIndex :: Days.Day
  let solveFn = chooseSolveFunction part day
  stdin <- getContents
  case solveFn stdin of
    Left error -> abort $ "Error: " <> error
    Right result -> putStrLn $ "Result: \n" <> result

main :: IO ()
main = do
  args <- getArgs
  args <- case parseArgs args of
    Left error -> abort error
    Right args -> return args
  start args
