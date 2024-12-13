module Direction(Direction(..), allDirections, directionAxis, directionX, directionY, turnLeft, turnRight) where
import Data.Array (Ix)

data Direction = DirDown | DirUp | DirLeft | DirRight deriving (Eq, Ord, Show, Bounded, Enum, Ix)

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

directionX :: Num a => Direction -> a
directionX DirRight = 1
directionX DirLeft = -1
directionX _ = 0

directionY :: Num a => Direction -> a
directionY DirDown = 1
directionY DirUp = -1
directionY _ = 0

