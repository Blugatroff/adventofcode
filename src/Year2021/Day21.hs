module Year2021.Day21 (partOne, partTwo) where

import MeLude
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState)
import Control.Monad.State.Lazy (State, evalState, get, gets, modify)
import Data.Map.Lazy qualified as Map
import Util (dedup, maximumOrZero, readInt, splitOnce, trim)

parse :: String -> Either String (Int, Int)
parse =
  (lines >>> map (trim isSpace) >>> filter (not . null) >>> traverse parseLine) >=> \case
    [a, b] -> Right (a, b)
    _ -> Left "expected only two players"

parseLine :: String -> Either String Int
parseLine line = do
  (_, numStr) <- maybeToEither ("failed to parse line: " <> line) $ splitOnce ':' line
  readInt $ trim isSpace numStr

data Player = Player {position :: Int, points :: Int}

data GameState = GameState {dice :: Int, playerA :: Player, playerB :: Player}

rollDice :: (MonadState GameState m) => m Int
rollDice = do
  modify $ \state -> state {dice = state.dice + 1}
  state <- get
  pure $ ((state.dice - 1) `mod` 100) + 1

playRound :: (MonadError Int m) => (MonadState GameState m) => m ()
playRound = do
  d1 <- rollDice
  d2 <- rollDice
  d3 <- rollDice
  let m1 = d1 + d2 + d3
  modify $ \state -> state {playerA = state.playerA {position = (state.playerA.position + m1) `mod` 10}}
  p1 <- gets (position . playerA)
  modify $ \state -> state {playerA = state.playerA {points = state.playerA.points + p1 + 1}}
  s1 <- gets (points . playerA)

  when (s1 >= 1000) $ do
    s2 <- gets (points . playerB)
    die <- gets dice
    throwError $ s2 * die

  d1 <- rollDice
  d2 <- rollDice
  d3 <- rollDice
  let m2 = d1 + d2 + d3
  modify $ \state -> state {playerB = state.playerB {position = (state.playerB.position + m2) `mod` 10}}
  p2 <- gets (position . playerB)
  modify $ \state -> state {playerB = state.playerB {points = state.playerB.points + p2 + 1}}
  s2 <- gets (points . playerB)
  when (s2 >= 1000) $ do
    die <- gets dice
    throwError $ s1 * die

solvePartOne :: (Int, Int) -> Either String String
solvePartOne (a, b) = do
  let initialState = GameState {playerA = Player {position = a - 1, points = 0}, playerB = Player {position = b - 1, points = 0}, dice = 0}
  case evalState (runExceptT $ forever playRound) initialState of
    Left result -> Right $ show result
    Right a -> absurd a

data Dirac = Dirac {value :: Int, frequency :: Integer}

possibleDiracs :: [Dirac]
possibleDiracs = map (\(value, frequency) -> Dirac {value, frequency = toInteger frequency}) $ dedup $ do
  let sides = [1, 2, 3]
  d1 <- sides
  d2 <- sides
  d3 <- sides
  pure $ d1 + d2 + d3

type Cache = Map ((Int, Int), (Int, Int)) (Integer, Integer)

playDirac :: Int -> (Player, Player) -> (Integer, Integer)
playDirac targetPoints (a, b) = flip evalState mempty $ go ((a.position, a.points), (b.position, b.points))
  where
    go :: ((Int, Int), (Int, Int)) -> State Cache (Integer, Integer)
    go arg@((aPosition, aPoints), (bPosition, bPoints)) =
      gets (Map.lookup arg) >>= \case
        Just a -> pure a
        Nothing -> do
          res <- fmap (addWinCounts . concat) $ do
            for possibleDiracs $ \d1 -> do
              let newPositionA = (aPosition + d1.value) `mod` 10
              let newPointsA = aPoints + newPositionA + 1
              if newPointsA >= targetPoints
                then pure [(d1.frequency, 0)]
                else do
                  let newA = (newPositionA, newPointsA)
                  for possibleDiracs $ \d2 -> do
                    let combinedFrequency = d1.frequency * d2.frequency
                    let newPositionB = (bPosition + d2.value) `mod` 10
                    let newPointsB = bPoints + newPositionB + 1
                    if newPointsB >= targetPoints
                      then pure (0, combinedFrequency)
                      else do
                        let newB = (newPositionB, newPointsB)
                        (winsA, winsB) <- go (newA, newB)
                        pure (winsA * combinedFrequency, winsB * combinedFrequency)
          modify (Map.insert arg res)
          pure res

    addWinCounts :: [(Integer, Integer)] -> (Integer, Integer)
    addWinCounts arr = (sum (map fst arr), sum (map snd arr))

solvePartTwo :: (Int, Int) -> Either String String
solvePartTwo (a, b) = do
  let initialState = (Player {position = a - 1, points = 0}, Player {position = b - 1, points = 0})
  let (winsA, winsB) = playDirac 21 initialState
  Right $ show $ maximumOrZero [winsA, winsB]

partOne :: String -> Either String String
partOne = parse >=> solvePartOne

partTwo :: String -> Either String String
partTwo = parse >=> solvePartTwo

