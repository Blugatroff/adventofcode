module Year2022.Day17 (partOne, partTwo) where

import Control.Monad (replicateM_, when)
import Control.Monad.State (State, evalState, execState, get, gets, modify)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor (void, ($>), (<&>))
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Util (dedup, mapFst, mapSnd, safeMaximum, safeMinimum)
import Data.Pos (Pos(..))

data Shape = Shape String (S.Set Pos)
  deriving (Eq)

instance Show Shape where
  show (Shape name _) = name

data Jet = LeftJet | RightJet
  deriving (Show)

data Jets = Jets Int [Jet]

takeJet :: Jets -> (Jet, Jets)
takeJet (Jets i jets) = (jets !! i, Jets ni jets)
  where
    ni = (i + 1) `mod` length jets

jetIndex :: Jets -> Int
jetIndex (Jets i _) = i

verticalLineShape :: Shape
verticalLineShape = Shape "VerticalLine" $ S.fromList [Pos 0 y | y <- [0 .. 3]]

horizontalLineShape :: Shape
horizontalLineShape = Shape "HorizontalLine" $ S.fromList [Pos x 0 | x <- [0 .. 3]]

crossShape :: Shape
crossShape = Shape "Cross" $ S.fromList $ [Pos 1 y | y <- [0 .. 2]] <> [Pos x 1 | x <- [0 .. 2]]

lShape :: Shape
lShape = Shape "L" $ S.fromList $ [Pos 2 y | y <- [0 .. 2]] <> [Pos x 0 | x <- [0 .. 2]]

squareShape :: Shape
squareShape = Shape "Square" $ S.fromList [Pos 0 0, Pos 0 1, Pos 1 0, Pos 1 1]

shapes :: [Shape]
shapes = cycle [horizontalLineShape, crossShape, lShape, verticalLineShape, squareShape]

newtype Chamber = Chamber (S.Set Pos)

emptyChamber :: Chamber
emptyChamber = Chamber S.empty

modifyChamber :: (S.Set Pos -> S.Set Pos) -> Chamber -> Chamber
modifyChamber f (Chamber s) = Chamber $ f s

instance Show Chamber where
  show (Chamber blocks) =
    intercalate "\n" $
      reverse [0 .. maxY] <&> \y ->
        [0 .. 6] <&> \x -> if S.member (Pos {x, y}) blocks then '#' else '.'
    where
      maxY = S.elems blocks <&> y & safeMaximum & fromMaybe 0

data TetrisState = TetrisState {stateChamber :: Chamber, stateShapes :: [Shape], stateCount :: Int, stateJets :: Jets}

modifyStateChamber :: (Chamber -> Chamber) -> TetrisState -> TetrisState
modifyStateChamber f (TetrisState {stateChamber, stateJets, stateShapes, stateCount}) = TetrisState {stateChamber = f stateChamber, stateJets, stateShapes, stateCount}

mapStateJets :: (Jets -> Jets) -> TetrisState -> TetrisState
mapStateJets f (TetrisState {stateChamber, stateJets, stateShapes, stateCount}) = TetrisState {stateChamber, stateJets = f stateJets, stateShapes, stateCount}

mapStateShapes :: ([Shape] -> [Shape]) -> TetrisState -> TetrisState
mapStateShapes f (TetrisState {stateChamber, stateJets, stateShapes, stateCount}) = TetrisState {stateChamber, stateJets, stateShapes = f stateShapes, stateCount}

mapStateCount :: (Int -> Int) -> TetrisState -> TetrisState
mapStateCount f (TetrisState {stateChamber, stateJets, stateShapes, stateCount}) = TetrisState {stateChamber, stateJets, stateShapes, stateCount = f stateCount}

incrementCount :: State TetrisState ()
incrementCount = modify $ mapStateCount (+ 1)

spawnShape :: Pos -> Shape -> State TetrisState ()
spawnShape pos (Shape _ blocks) =
  S.elems blocks
    <&> (+ pos)
    & filter (\(Pos {x, y}) -> y >= 0 && x >= 0 && x <= 6)
    & traverse_ (modify . modifyStateChamber . modifyChamber . S.insert)

collisionWith :: Pos -> Shape -> Chamber -> Bool
collisionWith pos (Shape _ shape) (Chamber blocks) = S.elems shape <&> (+ pos) & any colliding
  where
    colliding pos@(Pos {x, y}) = x < 0 || x > 6 || y < 0 || S.member pos blocks

takeStateJet :: State TetrisState Jet
takeStateJet = do
  (jet, jets) <- gets $ takeJet . stateJets
  modify (mapStateJets $ const jets) $> jet

peekShape :: State TetrisState Shape
peekShape =
  gets stateShapes >>= \case
    (shape : shapes) -> return shape
    _ -> undefined

takeShape :: State TetrisState Shape
takeShape =
  gets stateShapes >>= \case
    (shape : shapes) -> modify (mapStateShapes $ const shapes) $> shape
    _ -> undefined

dropShape :: Pos -> Shape -> State TetrisState Bool
dropShape pos@(Pos {x, y}) shape = do
  jets <- gets stateJets
  chamber <- gets stateChamber
  if collisionWith pos shape chamber
    then return False
    else do
      jet <- takeStateJet
      let testX = case jet of LeftJet -> x - 1; RightJet -> x + 1
      let newX = if collisionWith (Pos {x = testX, y}) shape chamber then x else testX
      if collisionWith (Pos {x = newX, y = y - 1}) shape chamber
        then do
          incrementCount
          spawnShape (Pos {x = newX, y = y}) shape $> True
        else dropShape (Pos {x = newX, y = y - 1}) shape

highestBlock :: Chamber -> Int
highestBlock (Chamber blocks) | S.null blocks = 0
highestBlock (Chamber blocks) = (1 +) $ y $ S.findMax blocks

parse :: String -> Jets
parse = Jets 0 . mapMaybe (\case '>' -> Just RightJet; '<' -> Just LeftJet; _ -> Nothing)

spawnAndDropShape :: State TetrisState ()
spawnAndDropShape = do
  shape <- takeShape
  highest <- gets (highestBlock . stateChamber)
  void $ dropShape (Pos {x = 2, y = highest + 3}) shape

solvePartOne :: Int -> State TetrisState Int
solvePartOne n = do
  replicateM_ n spawnAndDropShape
  gets (highestBlock . stateChamber)

newtype Seal = Seal (S.Set Pos)
  deriving (Eq)

instance Show Seal where
  show (Seal seal) =
    intercalate "\n" $
      reverse [minY .. maxY] <&> \y ->
        [minX .. maxX] <&> \x ->
          if S.member (Pos {x, y}) seal then '#' else '.'
    where
      minX = S.elems seal <&> x & safeMinimum & fromMaybe 0
      maxX = S.elems seal <&> x & safeMaximum & fromMaybe 0
      minY = S.elems seal <&> y & safeMinimum & fromMaybe 0
      maxY = S.elems seal <&> y & safeMaximum & fromMaybe 0

normalizeSeal :: Seal -> Seal
normalizeSeal (Seal seal) = Seal $ S.fromList $ subtract (Pos {x = minX, y = minY}) <$> S.elems seal
  where
    minX = S.elems seal <&> x & safeMinimum & fromMaybe 0
    minY = S.elems seal <&> y & safeMinimum & fromMaybe 0

flood :: Chamber -> Maybe Seal
flood chamber@(Chamber blocks) =
  if (7 ==) $ length $ dedup $ x <$> S.elems seal
    then Just (normalizeSeal $ Seal seal)
    else Nothing
  where
    seal = snd $ execState (flow (Pos {x = 0, y = highest})) (S.empty, S.empty)

    highest = highestBlock chamber

    flow :: Pos -> State (S.Set Pos, S.Set Pos) ()
    flow (Pos {x, y}) | y < 0 || y > highest = return ()
    flow pos@(Pos {x, y}) = do
      visited <- gets fst
      seal <- gets snd
      if S.member pos seal || S.member pos visited
        then return ()
        else do
          modify $ mapFst $ S.insert pos
          if S.member pos blocks
            then modify $ mapSnd $ S.insert pos
            else do
              flow (Pos {x, y = y + 1})
              flow (Pos {x, y = y - 1})
              when (x > 0) $ flow (Pos {x = x - 1, y})
              when (x < 6) $ flow (Pos {x = x + 1, y})

untilSeal :: State TetrisState Seal
untilSeal = do
  seal <- gets $ flood . stateChamber
  void spawnAndDropShape
  maybe untilSeal return seal

findSealPair :: [(Seal, Shape, Int, TetrisState)] -> State TetrisState (Seal, Shape, Int, TetrisState)
findSealPair previousSeals = do
  void spawnAndDropShape
  nextSeal <- untilSeal
  nextShape <- peekShape
  nextJetIndex <- gets $ jetIndex . stateJets
  case find (\(seal, shape, jetIndex, _) -> seal == nextSeal && shape == nextShape && jetIndex == nextJetIndex) previousSeals of
    Just p -> return p
    Nothing -> do
      state <- get
      findSealPair ((nextSeal, nextShape, nextJetIndex, state) : previousSeals)

solvePartTwo :: Int -> State TetrisState String
solvePartTwo n = do
  jets <- gets stateJets
  (seal, shape, _, previousState) <- findSealPair []
  chamber <- gets stateChamber
  let h1 = highestBlock $ stateChamber previousState
  state <- get
  let h2 = highestBlock $ stateChamber state
  let hd = h2 - h1
  let c1 = stateCount previousState
  let c2 = stateCount state
  let cd = c2 - c1

  let repeats = (n - c1) `div` cd
  let skippedSteps = repeats * cd
  let skippedHeight = repeats * hd
  let remainingSteps = n - skippedSteps - c1
  let remainingAndFirstSteps = c1 + remainingSteps

  let h = evalState (solvePartOne remainingAndFirstSteps) $ TetrisState emptyChamber shapes 0 jets

  return $ show $ h + skippedHeight

partOne :: String -> Either String String
partOne = Right . show . evalState (solvePartOne 2022) . TetrisState emptyChamber shapes 0 . parse

partTwo :: String -> Either String String
partTwo = Right . evalState (solvePartTwo 1000000000000) . TetrisState emptyChamber shapes 0 . parse
