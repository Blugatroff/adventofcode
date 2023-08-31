module Year2022.Day25 (partOne, partTwo) where
import Util (split)
import Data.Traversable (for)

data SnafuDigit = Two | One | Zero | Minus | DoubleMinus

instance Show SnafuDigit where
  show = \case 
    Two -> "2"
    One -> "1"
    Zero -> "0"
    Minus -> "-"
    DoubleMinus -> "="

parse :: String -> Either String [[SnafuDigit]]
parse input = do
  for (filter (not . null) $ split '\n' input) $ \line -> do
    for line $ \case
      '2' -> Right Two
      '1' -> Right One
      '0' -> Right Zero
      '-' -> Right Minus
      '=' -> Right DoubleMinus
      cha -> Left $ "invalid digit: " <> show cha

snafuDigitToInt :: SnafuDigit -> Integer
snafuDigitToInt = \case
  Two -> 2
  One -> 1
  Zero -> 0
  Minus -> -1
  DoubleMinus -> -2

snafuToInt :: [SnafuDigit] -> Integer
snafuToInt = foldl fold 0
  where
  fold n digit = n * 5 + snafuDigitToInt digit

intToSnafu :: Integer -> [SnafuDigit]
intToSnafu n | n <= 2 && n >= -2 = case n of
  2 -> [ Two ]
  1 -> [ One ]
  0 -> [ Zero ]
  -1 -> [ Minus ]
  -2 -> [ DoubleMinus ]
  _ -> []
intToSnafu n = do
  let left = (n + 2) `div` 5
  let right = n - left * 5
  intToSnafu left <> intToSnafu right

showSnafu :: forall f. Foldable f => f SnafuDigit -> String
showSnafu = foldMap show

solvePartOne :: [[SnafuDigit]] -> String
solvePartOne snafus = showSnafu (intToSnafu (sum (map snafuToInt snafus)))

partOne :: String -> Either String String
partOne = fmap solvePartOne <$> parse

partTwo :: String -> Either String String
partTwo = const $ Right "This puzzle doesn't have a second part"

