module Direction (Direction (..), allDirections, directionAxis, turnLeft, turnRight) where

import MeLude
import GHC.Records (HasField (..))

data Direction = DirDown | DirUp | DirLeft | DirRight deriving (Eq, Ord, Show, Bounded, Enum, Ix)

instance HasField "x" Direction Int where
  getField = \case
    DirDown -> 0
    DirUp -> 0
    DirLeft -> -1
    DirRight -> 1

instance HasField "y" Direction Int where
  getField = \case
    DirDown -> 1
    DirUp -> -1
    DirLeft -> 0
    DirRight -> 0

allDirections :: [Direction]
allDirections = [DirUp, DirRight, DirDown, DirLeft]

directionAxis :: Direction -> (a, a) -> a
directionAxis DirDown = snd
directionAxis DirUp = snd
directionAxis DirRight = fst
directionAxis DirLeft = fst

turnLeft :: Direction -> Direction
turnLeft DirUp = DirLeft
turnLeft DirLeft = DirDown
turnLeft DirDown = DirRight
turnLeft DirRight = DirUp

turnRight :: Direction -> Direction
turnRight DirUp = DirRight
turnRight DirRight = DirDown
turnRight DirDown = DirLeft
turnRight DirLeft = DirUp
