module Main (main) where

import Data.Functor ((<&>))
import Data.List (find)
import Day (Day (partOne, partTwo), Part, Year (days, name))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import Util (readInt)
import qualified Year2021
import qualified Year2022
import Control.Exception (try)
import Control.Monad (forM_)

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

validateDay :: [Day] -> Int -> Either String DayIndex
validateDay days day
    | day - 1 >= length days || day <= 0 = Left $ "day must be above 0 and below " <> show (length days + 1)
validateDay _ day = Right $ DayIndex $ day - 1

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
parseArgs ("--stdin" : rest) = parseArgs rest <&> \case 
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
    [ Year2021.year
    , Year2022.year
    ]

start :: Args -> IO ()
start Help = do
    putStrLn "Advent Of Code in Haskell"
    putStrLn ""
    putStrLn "Usage: ./aoc [--stdin] <year> <day> <part>"
start (SpecificDay year day part source) = runDay source year day part
start (AllFromYear year part) = do
    forM_ (zip [0 :: Int ..] $ days year) $ \(index, day) -> do
        putStrLn $ "Day " <> show (index + 1) <> ":"
        runDay File year (DayIndex index) part
  
loadFromSource :: Year -> DayIndex -> InputSource -> IO String
loadFromSource year day Stdin = getContents
loadFromSource year (DayIndex dayIndex) File = do
    let path = "./inputs/" <> show year <> "/" <> show (dayIndex + 1) <> ".txt"
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
    let day = days year !! dayIndex
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

