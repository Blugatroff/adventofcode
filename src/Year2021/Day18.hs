module Year2021.Day18 (partOne, partTwo) where

import Control.Applicative ((<|>))
import Control.Monad.State (StateT, evalStateT, get, lift, put)
import Control.Monad.State.Class (MonadState)
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Util
  ( mapFst,
    readInt,
    reduceL,
    safeHead,
    safeLast,
    split,
    safeMaximum
  )

data Element = Number !Int | Pair !Element !Element
  deriving (Eq)

instance Show Element where
  show (Number n) = show n
  show (Pair left right) = "[" <> show left <> "," <> show right <> "]"

parseChar :: Char -> StateT String (Either String) Char
parseChar char =
  get >>= \case
    [] -> lift $ Left $ "expected " <> [char] <> " found empty"
    (c : rest)
      | c == char -> do
        put rest
        return c
      | otherwise -> lift $ Left $ "expected " <> [char] <> " found: " <> [c]

throwInState :: e -> StateT s (Either e) a
throwInState error = lift $ Left error

parseInt :: StateT String (Either String) Int
parseInt =
  get >>= \case
    [] -> throwInState "expected number found empty"
    (first : rest) | isDigit first -> do
      number <- takeFromStateWhile isDigit
      lift $ readInt number
    rest -> throwInState $ "expected number found: " <> rest

takeFromStateWhile :: (MonadState [a] m) => (a -> Bool) -> m [a]
takeFromStateWhile predicate =
  get >>= \case
    [] -> return []
    (next : rest)
      | predicate next -> do
        put rest
        takeFromStateWhile predicate <&> (next :)
    nonMatching -> return []

parseElement :: StateT String (Either String) Element
parseElement =
  get >>= \case
    [] -> throwInState "Expected element found empty"
    ('[' : rest) -> do
      put rest
      elem1 <- parseElement
      _ <- parseChar ','
      elem2 <- parseElement
      _ <- parseChar ']'
      return $ Pair elem1 elem2
    (first : rest)
      | isDigit first -> Number <$> parseInt
      | otherwise -> throwInState $ "Expected element found: " <> (first : rest)

parse :: String -> Either String [Element]
parse input = traverse (evalStateT parseElement) (filter (not . null) $ split '\n' input)

data Location = Here | LeftSide !Location | RightSide !Location
  deriving (Show, Eq)

instance Ord Location where
  compare Here Here = EQ
  compare (LeftSide _) Here = LT
  compare (RightSide _) Here = GT
  compare Here (LeftSide _) = GT
  compare Here (RightSide _) = LT
  compare (LeftSide left) (RightSide right) = compare left right
  compare (RightSide right) (LeftSide left) = compare right left
  compare (LeftSide a) (LeftSide b) = compare a b
  compare (RightSide a) (RightSide b) = compare a b

inDepth :: Int -> Element -> [(Location, Element)]
inDepth 0 element = [(Here, element)]
inDepth depth (Number _) = []
inDepth depth (Pair left right) =
  (mapFst LeftSide <$> inDepth (depth - 1) left)
    ++ (mapFst RightSide <$> inDepth (depth - 1) right)

getElement :: Location -> Element -> Maybe Element
getElement Here element = Just element
getElement (LeftSide loc) (Number n) = Nothing
getElement (RightSide loc) (Number n) = Nothing
getElement (LeftSide loc) (Pair left right) = getElement loc left
getElement (RightSide loc) (Pair left right) = getElement loc right

replaceElement :: Location -> (Element -> Element) -> Element -> (Element, Maybe Element)
replaceElement Here new tree = (new tree, Just tree)
replaceElement (RightSide loc) new (Number n) = (Number n, Nothing)
replaceElement (LeftSide loc) new (Number n) = (Number n, Nothing)
replaceElement (LeftSide loc) new (Pair left right) = replaceElement loc new left & mapFst (`Pair` right)
replaceElement (RightSide loc) new (Pair left right) = replaceElement loc new right & mapFst (Pair left)

explode :: Location -> Element -> Element
explode location element = case explodingPair of
  Just (Pair (Number left) (Number right)) ->
    replaceExploding $ addLeftValue left $ addRightValue right element
  nonRegularNumberPair -> element
  where
    add :: Int -> Element -> Element
    add v (Number n) = Number $ n + v
    add v el = el

    explodingPair = getElement location element

    replaceExploding :: Element -> Element
    replaceExploding = fst . replaceElement location (const (Number 0))

    addRightValue :: Int -> Element -> Element
    addRightValue value element =
      firstRight
        <&> (\firstRight -> replaceElement firstRight (add value) element & fst)
        & fromMaybe element

    addLeftValue :: Int -> Element -> Element
    addLeftValue value element =
      firstLeft
        <&> (\firstLeft -> replaceElement firstLeft (add value) element & fst)
        & fromMaybe element

    firstLeft = firstNumberToTheLeft location element
    firstRight = firstNumberToTheRight location element

    firstNumberToTheLeft :: Location -> Element -> Maybe Location
    firstNumberToTheLeft _ (Number n) = Nothing
    firstNumberToTheLeft Here (Pair left right) = Nothing
    firstNumberToTheLeft (RightSide loc) (Pair (Number n) right) =
      (firstNumberToTheLeft loc right <&> RightSide) <|> Just (LeftSide Here)
    firstNumberToTheLeft (RightSide loc) (Pair left right) =
      (firstNumberToTheLeft loc right <&> RightSide) <|> lastNumberInLeftElement
      where
        lastNumberInLeftElement :: Maybe Location
        lastNumberInLeftElement =
          elementsWithLocation left & filter (isNumber . snd)
            <&> fst & safeLast
            <&> LeftSide
    firstNumberToTheLeft (LeftSide loc) (Pair left right) =
      firstNumberToTheLeft loc left <&> LeftSide

    firstNumberToTheRight :: Location -> Element -> Maybe Location
    firstNumberToTheRight _ (Number n) = Nothing
    firstNumberToTheRight Here (Pair left right) = Nothing
    firstNumberToTheRight (LeftSide loc) (Pair left (Number n)) =
      (firstNumberToTheRight loc left <&> LeftSide) <|> Just (RightSide Here)
    firstNumberToTheRight (LeftSide loc) (Pair left right) =
      (firstNumberToTheRight loc left <&> LeftSide) <|> firstNumberInRightElement
      where
        firstNumberInRightElement :: Maybe Location
        firstNumberInRightElement =
          elementsWithLocation right & filter (isNumber . snd)
            <&> fst & safeHead
            <&> RightSide
    firstNumberToTheRight (RightSide loc) (Pair left right) =
      firstNumberToTheRight loc right <&> RightSide

splitElement :: Location -> Element -> Element
splitElement location = fst . replaceElement location splitter
  where
    splitter :: Element -> Element
    splitter (Number n) = Pair (Number (n `div` 2)) (Number (n - (n `div` 2)))
    splitter element = element

isPair :: Element -> Bool
isPair (Pair left right) = True
isPair (Number n) = False

isNumber :: Element -> Bool
isNumber (Number n) = True
isNumber pair = False

elementsWithLocation :: Element -> [(Location, Element)]
elementsWithLocation el@(Number n) = [(Here, el)]
elementsWithLocation el@(Pair left right) =
  (Here, el) :
  (mapFst LeftSide <$> elementsWithLocation left)
    ++ (mapFst RightSide <$> elementsWithLocation right)

findBigRegularNumbers :: Element -> [Location]
findBigRegularNumbers = map fst . filter (test . snd) . elementsWithLocation
  where
    test :: Element -> Bool
    test (Number n) | n >= 10 = True
    test notABigNumber = False

reduceElement :: Element -> Element
reduceElement element = maybe element reduceElement (step element)
  where
    step :: Element -> Maybe Element
    step element = exploded <|> splitted
      where
        exploding = safeHead (map fst $ filter (isPair . snd) $ inDepth 4 element)
        exploded = exploding <&> (`explode` element)

        toSplit = safeHead $ findBigRegularNumbers element
        splitted = toSplit <&> (`splitElement` element)

addElements :: Element -> Element -> Element
addElements a b = reduceElement $ Pair a b

magnitude :: Element -> Int
magnitude (Number n) = n
magnitude (Pair left right) = 3 * magnitude left + 2 * magnitude right

solvePartOne :: [Element] -> Int
solvePartOne elements = reduceL addElements elements & maybe 0 magnitude

everyPair :: [a] -> [(a, a)]
everyPair elems = elems >>= (\elem -> elems <&> (elem,))

solvePartTwo :: [Element] -> Int
solvePartTwo elements =
  everyPair elements
    & map (magnitude . uncurry addElements)
    & safeMaximum
    & fromMaybe 0

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
