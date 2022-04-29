import Data.List
import Util

data Slot = Slot Bool Int

instance Show Slot where
  show (Slot marked n) = show n ++ (if marked then "+" else "-")

newtype Board = Board [[Slot]]

slots :: Board -> [[Slot]]
slots (Board slots) = slots

transposeBoard :: Board -> Board
transposeBoard (Board board) = Board $ transpose board

showPadded :: Show a => Int -> a -> String
showPadded l a = lpad l ' ' $ show a

showListPadded :: Show a => Int -> [a] -> String
showListPadded n l = '[' : intercalate "," (map (showPadded n) l) ++ "]"

instance Show Board where
  show (Board slots) = "Board [\n" ++ concatMap ('\t' :) (intersperse "\n" $ map (showListPadded 2) slots) ++ "\n]"

data Game = Game [Int] [Board]
  deriving (Show)

replaceUntilNoChange :: Eq a => [a] -> [a] -> [a] -> [a]
replaceUntilNoChange pat with list = if new == list then new else replaceUntilNoChange pat with new
  where
    new = replace pat with list

chunksOf :: Int -> [a] -> [[a]]
chunksOf size list = if null b then [a] else a : chunksOf size b
  where
    (a, b) = splitAt size list

parseBoard :: [String] -> Board
parseBoard = Board . map (map (Slot False . read) . split ' ' . replaceUntilNoChange "  " " ")

trimSpaces :: String -> String
trimSpaces = trim (== ' ')

parse :: String -> Game
parse s = Game draws boards
  where
    lines = filter (not . null) $ split '\n' s
    (drawsLine : boardLines) = lines
    draws = map read $ split ',' drawsLine
    boards = map parseBoard $ chunksOf 5 $ map (trim (== ' ')) boardLines

playRound :: Int -> Board -> Board
playRound draw = Board . map (map mapSlot) . slots
  where
    mapSlot :: Slot -> Slot
    mapSlot (Slot m n) = Slot (m || (n == draw)) n

marked :: Slot -> Bool
marked (Slot marked _) = marked

hasWonHorizontal :: Board -> Bool
hasWonHorizontal (Board slots) = any (all marked) slots

hasWon :: Board -> Bool
hasWon board = hasWonHorizontal board || hasWonHorizontal (transposeBoard board)

slotNumber :: Slot -> Int
slotNumber (Slot _ n) = n

computeScore :: Int -> Board -> Int
computeScore draw board = draw * sum (map slotNumber $ filter (not . marked) $ concat $ slots board)

playLoser :: Game -> Maybe (Board, Int, [Int])
playLoser (Game [] boards) = Nothing
playLoser (Game (draw : draws) boards) =
  case notWon of
    [loser] -> Just (loser, draw, draws)
    [] -> error "no loser found"
    remaining -> playLoser (Game draws remaining)
  where
    notWon = filter (not . hasWon) $ map (playRound draw) boards

play :: Game -> Maybe (Board, Int)
play (Game [] boards) = Nothing
play (Game (draw : draws) boards) =
  case winningBoard of
    Nothing -> do
      play (Game draws newBoards)
    Just winner -> do
      Just (winner, draw)
  where
    newBoards = map (playRound draw) boards
    winningBoard = find hasWon newBoards

solve :: Game -> Int
solve game = case winner of
  Nothing -> error "no winner found"
  Just (board, draw) -> computeScore draw board
  where
    winner = play game

solve2 :: Game -> Int
solve2 game = case loser of
  Nothing -> error "not loser found"
  Just (board, draw, draws) -> solve $ Game draws [board]
  where
    loser = playLoser game

main :: IO ()
main = interact $ show . solve2 . parse
