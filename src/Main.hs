module Main (main) where

import Configuration.Dotenv qualified as Dotenv
import Control.Exception
import Control.Monad (forM_, void, forM)
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.List (find, sort)
import Data.Map qualified as M
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Day (Day (partOne, partTwo), Part, Year (days, name))
import Network.HTTP.Simple qualified as HTTPSimple
import Network.HTTP.Types qualified as HTTPTypes
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)

import Util (Unwrap (unwrap), readInt, trim)
import Year2021 qualified
import Year2022 qualified
import Control.Parallel.Strategies

data PartName = PartOne | PartTwo | Both
  deriving (Show)

newtype DayIndex = DayIndex Int deriving (NFData)

instance Show DayIndex where
  show (DayIndex i) = show i

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
  "both" -> Right Both
  "all" -> Right Both
  "12" -> Right Both
  part -> Left $ "unexpected part '" <> part <> "' use either 'one', 'two', '1' or '2'"

parseArgs :: [String] -> Either String Args
parseArgs [] = Right Help
parseArgs ["--help"] = Right Help
parseArgs ("--stdin" : rest) =
  parseArgs rest <&> \case
    SpecificDay y d p _ -> SpecificDay y d p Stdin
    a -> a
parseArgs [year, day] = parseArgs [year, day, "both"]
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
chooseSolveFunction Both day = \input -> do
  p1 <- partOne day input
  p2 <- partTwo day input
  Right $ p1 <> " \n" <> p2


years :: [Year]
years =
  [ Year2021.year,
    Year2022.year
  ]

cookie :: String
cookie = "COOKIE"

loadInput :: Year -> DayIndex -> IO String
loadInput year day = do
  Dotenv.onMissingFile (void $ Dotenv.loadFile Dotenv.defaultConfig) (return ())
  session <- lookupEnv cookie
  session <- case session of
    Just session -> return session
    Nothing -> do
      hPutStrLn stderr (cookie <> " environment variable missing")
      exitFailure
  let url = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"
  req <- HTTPSimple.parseRequest url
  let reqWithHeader = HTTPSimple.setRequestHeader HTTPTypes.hCookie [encodeUtf8 ("session=" <> Text.pack session)] req
  response <- catch (HTTPSimple.httpBS reqWithHeader) $ \(exception :: HTTPSimple.HttpException) -> do
    hPrint stderr exception
    exitFailure
  let body = Text.unpack $ decodeUtf8 $ HTTPSimple.getResponseBody response
  case HTTPSimple.getResponseStatusCode response of
    200 -> pure body
    400 -> do
      hPutStrLn stderr $ "Server responded with 400 is your session cookie still valid? (" <> trim isSpace body <> ")"
      exitFailure
    code -> do
      hPutStrLn stderr body
      exitFailure

start :: Args -> IO ()
start Help = do
  putStrLn "Advent Of Code in Haskell"
  putStrLn ""
  putStrLn "Usage: ./aoc [--stdin] <year> <day> <part>"
start (SpecificDay year day part source) = runDayAndPrintResult source year day part
start (AllFromYear year part) = do
  let daysInOrder = map DayIndex $ sort $ M.keys $ days year
  runDaysInParallel year part daysInOrder

runDaysInParallel :: Year -> PartName -> [DayIndex] -> IO ()
runDaysInParallel year part days = do
  results <- forM days $ \day -> do
    result <- runDay File year day part
    pure (day, result)
  forM_ (results `using` parList rdeepseq) $ \(day, result) -> do
    putStrLn $ "Day " <> show day <> ":"
    case result of
      Left e -> putStrLn $ "Error: " <> e
      Right result -> putStrLn $ "Result: \n" <> result

loadFromSource :: Year -> DayIndex -> InputSource -> IO String
loadFromSource year day Stdin = getContents
loadFromSource year day File = do
  let directoryPath = "./inputs/" <> show year
  let path = directoryPath <> "/" <> show day <> ".txt"
  result <- try (readFile path) :: IO (Either IOError String)
  case result of
    Right contents -> return contents
    Left e | isDoesNotExistError e -> do
      input <- loadInput year day
      createDirectoryIfMissing True directoryPath
      writeFile path input
      pure input
    Left _ -> do
      putStrLn $ "Failed to read the file: \"" <> path <> "\"!"
      exitFailure

runDay :: InputSource -> Year -> DayIndex -> PartName -> IO (Either String String)
runDay source year day@(DayIndex dayIndex) part = do
  input <- loadFromSource year day source
  let day = unwrap $ M.lookup dayIndex $ days year
  let solveFn = chooseSolveFunction part day
  pure $ solveFn input

runDayAndPrintResult :: InputSource -> Year -> DayIndex -> PartName -> IO ()
runDayAndPrintResult source year day part =
  runDay source year day part >>= \case
    Left e -> abort $ "Error: " <> e
    Right result -> putStrLn $ "Result: \n" <> result

main :: IO ()
main = do
  args <- getArgs
  args <- case parseArgs args of
    Left error -> abort error
    Right args -> return args
  start args
