module Main (main) where

import Control.Exception (try)
import Control.Monad (forM_)
import Data.Function
import Data.Functor ((<&>))
import Data.List (find, sortBy)
import Data.Map qualified as M
import Day (Day (partOne, partTwo), Part, Year (days, name))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import Util (Unwrap (unwrap), readInt)
import Year2021 qualified
import Year2022 qualified

data PartName = PartOne | PartTwo
  deriving (Show)

newtype DayIndex = DayIndex Int
  deriving (Show)

data InputSource = Stdin | File
  deriving (Show)

data Args
  = Help
  | SpecificDay !Year !DayIndex !PartName !InputSource
  | AllFromYear !Year !PartName
  deriving (Show)

validateDay :: M.Map Int Day -> Int -> Either String DayIndex
validateDay days day
  | M.member day days = Right $ DayIndex day
validateDay _ day = Left $ "The day " <> show day <> " does not exist! (yet?)"

validateYear :: Int -> Either String Year
validateYear yearName = case find (\year -> name year == yearName) years of
  Just year -> Right year
  Nothing -> Left $ "There are no solutions for the year " <> show yearName <> " valid options include: " <> show (map name years)

parsePart :: String -> Either String PartName
parsePart = \case
  "one" -> Right PartOne
  "two" -> Right PartTwo
  "1" -> Right PartOne
  "2" -> Right PartTwo
  part -> Left $ "unexpected part '" <> part <> "' use either 'one', 'two', '1' or '2'"

parseArgs :: [String] -> Either String Args
parseArgs [] = Right Help
parseArgs ["--help"] = Right Help
parseArgs ("--stdin" : rest) =
  parseArgs rest <&> \case
    SpecificDay y d p _ -> SpecificDay y d p Stdin
    a -> a
parseArgs [year, day] = parseArgs [year, day, "one"]
parseArgs [year, day, part] = do
  year <- readInt year >>= validateYear
  case day of
    "all" -> do
      part <- parsePart part
      return $ AllFromYear year part
    day -> do
      day <- readInt day >>= validateDay (days year)
      part <- parsePart part
      return $ SpecificDay year day part File
parseArgs _ = Left "unexpected arguments, use --help"

abort :: String -> IO a
abort error = hPutStrLn stderr error >> exitFailure

chooseSolveFunction :: PartName -> Day -> Part
chooseSolveFunction PartOne day = partOne day
chooseSolveFunction PartTwo day = partTwo day

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
start (SpecificDay year day part source) = runDay source year day part
start (AllFromYear year part) = do
  forM_ (sortBy (compare `on` fst) $ M.assocs $ days year) $ \(index, day) -> do
    putStrLn $ "Day " <> show index <> ":"
    runDay File year (DayIndex index) part

loadFromSource :: Year -> DayIndex -> InputSource -> IO String
loadFromSource year day Stdin = getContents
loadFromSource year (DayIndex dayIndex) File = do
  let path = "./inputs/" <> show year <> "/" <> show dayIndex <> ".txt"
  result <- try (readFile path) :: IO (Either IOError String)
  case result of
    Right contents -> return contents
    Left e | isDoesNotExistError e -> do
      putStrLn $ "Attempted to read the puzzle input from \"" <> path <> " but the file does not exist!"
      exitFailure
    Left _ -> do
      putStrLn $ "Failed to read the file: \"" <> path <> "\"!"
      exitFailure

runDay :: InputSource -> Year -> DayIndex -> PartName -> IO ()
runDay source year day@(DayIndex dayIndex) part = do
  input <- loadFromSource year day source
  let day = unwrap $ M.lookup dayIndex $ days year
  let solveFn = chooseSolveFunction part day
  case solveFn input of
    Left e -> abort $ "Error: " <> e
    Right result -> putStrLn $ "Result: \n" <> result

main :: IO ()
main = do
  args <- getArgs
  args <- case parseArgs args of
    Left error -> abort error
    Right args -> return args
  start args
