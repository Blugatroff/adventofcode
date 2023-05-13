module Main (main) where

import Configuration.Dotenv qualified as Dotenv
import Control.Exception
import Control.Monad (forM_, void)
import Data.Function
import Data.Functor ((<&>))
import Data.List (find, sortBy)
import Data.Map qualified as M
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Day (Day (partOne, partTwo), Part, Year (days, name))
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Req ((/:))
import Network.HTTP.Req qualified as HTTPReq
import Network.HTTP.Types.Status (Status (..))
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Directory (createDirectoryIfMissing)
import Util (Unwrap (unwrap), readInt)
import Year2021 qualified
import Year2022 qualified

data PartName = PartOne | PartTwo
  deriving (Show)

newtype DayIndex = DayIndex Int

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
  let yearText = Text.pack $ show year
  let dayText = Text.pack $ show day
  res <- try $ HTTPReq.runReq HTTPReq.defaultHttpConfig $ do
    let sessionCookie = HTTPReq.header (encodeUtf8 "Cookie") (encodeUtf8 ("session=" <> Text.pack session))
    let url = HTTPReq.http "adventofcode.com" /: yearText /: "day" /: dayText /: "input"
    HTTPReq.responseBody <$> HTTPReq.req HTTPReq.GET url HTTPReq.NoReqBody HTTPReq.bsResponse sessionCookie
  body <- case res of
    Right body -> return body
    Left (error :: HTTPReq.HttpException) -> do
      error <- case error of
        HTTPReq.VanillaHttpException (HTTPClient.HttpExceptionRequest _ error) -> return error
        e -> do hPrint stderr e; exitFailure
      (Status code message) <- case error of
        HTTPClient.StatusCodeException response _ -> return $ HTTPClient.responseStatus response
        e -> do hPrint stderr e; exitFailure
      case code of
        400 -> do
          hPutStrLn stderr $ "Server responded with 400 " <> show message <> ", is your session cookie still valid?"
          exitFailure
        code -> do
          hPrint stderr message
          exitFailure
  return $ Text.unpack $ decodeUtf8 body

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
