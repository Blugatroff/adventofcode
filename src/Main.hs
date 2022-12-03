module Main (main) where

import Control.Exception (SomeException (SomeException), try)
import Control.Monad (void)
import Data.Either.Combinators (mapLeft)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe
import Day (Day (partOne, partTwo), Part, Year (days, name))
import GHC.IO.Exception (IOException (IOError))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import Text.Read (readEither)
import Util (readInt)
import qualified Year2021
import qualified Year2022

data PartName = PartOne | PartTwo
  deriving (Show)

newtype DayIndex = DayIndex Int
  deriving (Show)

data InputSource = Stdin | File
  deriving (Show)

data Args
  = Help
  | Args
      { yearIndex :: !Year,
        dayIndex :: !DayIndex,
        partName :: !(Maybe PartName),
        inputSource :: !InputSource
      }
  deriving (Show)

validateDay :: [Day] -> Int -> Either String DayIndex
validateDay days day
  | day - 1 >= length days || day <= 0 =
    Left $ "day must be above 0 and below " <> show (length days + 1)
validateDay days day = Right $ DayIndex $ day - 1

validateYear :: Int -> Either String Year
validateYear yearName = case find (\year -> name year == yearName) years of
  Just year -> Right year
  Nothing -> Left $ "There are no solutions for the year " <> show yearName <> " valid options include: " <> show (map name years)

parseArgs :: [String] -> Either String Args
parseArgs [] = Right Help
parseArgs ["--help"] = Right Help
parseArgs ("--stdin" : rest) = parseArgs rest <&> \args -> args {inputSource = Stdin}
parseArgs [year, day] = do
  year <- readInt year >>= validateYear
  day <- readInt day >>= validateDay (days year)
  return $ Args year day Nothing File
parseArgs [year, day, part] = do
  year <- readInt year >>= validateYear
  day <- readInt day >>= validateDay (days year)
  part <- case part of
    "one" -> Right PartOne
    "two" -> Right PartTwo
    "1" -> Right PartOne
    "2" -> Right PartTwo
    part -> Left $ "unexpected part '" <> part <> "' use either 'one', 'two', '1' or '2'"
  return $ Args year day (Just part) File
parseArgs _ = Left "unexpected arguments, use --help"

abort :: String -> IO a
abort error = hPutStrLn stderr error >> exitFailure

chooseSolveFunction :: Maybe PartName -> Day -> Part
chooseSolveFunction part day = case fromMaybe PartOne part of
  PartOne -> partOne day
  PartTwo -> partTwo day

years :: [Year]
years =
  [ Year2021.year,
    Year2022.year
  ]

start :: Args -> IO ()
start Help = do
  putStrLn "Advent Of Code in Haskell"
  putStrLn ""
  putStrLn "Usage: ./aoc [--stdin] <year> <day> <part>"
start (Args year (DayIndex dayIndex) part source) = do
  let day = days year !! dayIndex :: Day
  let solveFn = chooseSolveFunction part day

  input <- case source of
    Stdin -> getContents
    File -> do
      let path = "./inputs/" <> show year <> "/" <> show (dayIndex + 1) <> ".txt"
      result <- try (readFile path) :: IO (Either IOError String)
      case result of
        Right contents -> return contents
        Left error | isDoesNotExistError error -> do
          putStrLn $ "Attempted to read the puzzle input from \"" <> path <> " but the file does not exist!"
          exitFailure
        Left error -> do
          putStrLn $ "Failed to read the file: \"" <> path <> "\"!"
          exitFailure

  case solveFn input of
    Left error -> abort $ "Error: " <> error
    Right result -> putStrLn $ "Result: \n" <> result

main :: IO ()
main = do
  args <- getArgs
  args <- case parseArgs args of
    Left error -> abort error
    Right args -> return args
  start args
