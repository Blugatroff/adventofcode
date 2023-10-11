module Year2021.Day23 (partOne, partTwo) where

import Control.Monad ((<=<))
import Data.Array (Array)
import Data.Array qualified as Array
import Data.Array.IArray ((!), (//))
import Data.Char (isAlpha)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Heap qualified as Heap
import Data.List (sortBy, transpose)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Traversable (for)
import Util (safeHead)

data Amphipod = A | B | C | D deriving (Eq, Ord)

instance Show Amphipod where
  show A = "A"
  show B = "B"
  show C = "C"
  show D = "D"

data Room = Room {inner :: Tile, outer :: Tile} deriving (Show, Eq, Ord)

data Rooms = Rooms
  { a :: Room,
    b :: Room,
    c :: Room,
    d :: Room
  }
  deriving (Show, Eq, Ord)

parse :: String -> Either String Rooms
parse input = do
  rooms <- traverse (traverse parseAmphipod) (transpose $ map (filter isAlpha) $ take 2 $ drop 2 $ lines input)
  rooms <- for rooms $ \case
    [inner, outer] -> Right $ Room (Occupied inner) (Occupied outer)
    room -> Left $ "failed to parse room: " <> show room
  case rooms of
    [a, b, c, d] -> Right $ Rooms a b c d
    _ -> Left "failed to parse rooms"
  where
    parseAmphipod 'A' = Right A
    parseAmphipod 'B' = Right B
    parseAmphipod 'C' = Right C
    parseAmphipod 'D' = Right D
    parseAmphipod char = Left $ "failed to prase amphipod: " <> show char

data Tile = Unoccupied | Occupied Amphipod deriving (Eq, Ord)

instance Show Tile where
  show Unoccupied = "."
  show (Occupied amphi) = show amphi

data Burrow = Burrow {rooms :: Rooms, hallway :: Array Int Tile} deriving (Eq, Ord)

instance Show Burrow where
  show (Burrow {rooms, hallway}) =
    fold
      [ "#############\n#",
        concatMap (show . snd) $ sortBy (compare `on` fst) $ Array.assocs hallway,
        "#\n###" <> show rooms.a.inner <> "#" <> show rooms.b.inner <> "#" <> show rooms.c.inner <> "#" <> show rooms.d.inner,
        "###\n  #" <> show rooms.a.outer <> "#" <> show rooms.b.outer <> "#" <> show rooms.c.outer <> "#" <> show rooms.d.outer,
        "#  \n  #########  "
      ]

data RoomSlot = Inner | Outer deriving (Show, Eq, Ord)

data Location = InHallway Int | InRoom Amphipod RoomSlot deriving (Show, Eq, Ord)

data Step = MoveIntoRoom Location Amphipod RoomSlot | MoveIntoHallway Location Amphipod Int deriving (Show, Eq, Ord)

amphipodCost :: Amphipod -> Int
amphipodCost = \case
  A -> 1
  B -> 10
  C -> 100
  D -> 1000

stepCost :: Step -> Int
stepCost (MoveIntoRoom src amphi slot) = amphipodCost amphi * distance src (InRoom amphi slot)
stepCost (MoveIntoHallway src amphi p) = amphipodCost amphi * distance src (InHallway p)

distance :: Location -> Location -> Int
distance (InHallway p1) (InHallway p2) = abs (p1 - p2)
distance (InRoom room Inner) d = 1 + distance (InHallway (roomIndex room)) d
distance d (InRoom room Inner) = 1 + distance d (InHallway (roomIndex room))
distance (InRoom room Outer) d = 2 + distance (InHallway (roomIndex room)) d
distance d (InRoom room Outer) = 2 + distance d (InHallway (roomIndex room))

possibleSteps :: Burrow -> [Step]
possibleSteps burrow = do
  location <- allLocations
  possibleStepsInLocation burrow location

roomIndex :: Amphipod -> Int
roomIndex = \case
  A -> 3
  B -> 5
  C -> 7
  D -> 9

possibleStepsInLocation :: Burrow -> Location -> [Step]
possibleStepsInLocation burrow location = do
  case location of
    InHallway p -> case burrow.hallway ! p of
      Occupied amphi -> case pathToRoomFree p amphi of
        Nothing -> []
        Just slot -> [MoveIntoRoom location amphi slot]
      Unoccupied -> []
    InRoom room slot -> case pickRoomSlot burrow room slot of
      Occupied amphi -> do
        let p = roomIndex room
            directlyMoveToRoomStep =
              if room == amphi
                then []
                else case burrow.hallway ! p of
                  Unoccupied -> case pathToRoomFree p amphi of
                    Nothing -> []
                    Just slot -> [MoveIntoRoom location amphi slot]
                  Occupied _ -> []
        case slot of
          Outer | room == amphi && (Occupied amphi == pickRoomSlot burrow room Inner) -> [] -- This Amphipod is already in their Room, no reason to leave
          Outer -> case pickRoomSlot burrow room Inner of
            Occupied _ -> []
            Unoccupied -> directlyMoveToRoomStep <> (MoveIntoHallway location amphi <$> possibleHallwayStepsFrom burrow (roomIndex room))
          -- This Amphipod is already in their Room and the the outer Slot of the room is also already occupied by a correct Amphipod.
          Inner | room == amphi && (case pickRoomSlot burrow amphi Outer of Unoccupied -> False; Occupied amphi -> amphi == room) -> []
          Inner -> directlyMoveToRoomStep <> (MoveIntoHallway location amphi <$> possibleHallwayStepsFrom burrow (roomIndex room))
      Unoccupied -> []
  where
    pathToRoomFree p room = case compare p (roomIndex room) of
      LT -> case burrow.hallway ! (p + 1) of
        Occupied _ -> Nothing
        Unoccupied -> pathToRoomFree (p + 1) room
      GT -> case burrow.hallway ! (p - 1) of
        Occupied _ -> Nothing
        Unoccupied -> pathToRoomFree (p - 1) room
      EQ -> case pickRoomSlot burrow room Inner of
        Occupied _ -> Nothing
        Unoccupied -> case pickRoomSlot burrow room Outer of
          Occupied _ -> Just Inner
          Unoccupied -> Just Outer

possibleHallwayStepsFrom :: Burrow -> Int -> [Int]
possibleHallwayStepsFrom burrow p = case burrow.hallway ! p of
  Occupied _ -> []
  Unoccupied -> p : goLeft (p - 1) ++ goRight (p + 1)
  where
    goLeft 0 = []
    goLeft p = case burrow.hallway ! p of
      Occupied _ -> []
      Unoccupied -> p : goLeft (p - 1)
    goRight 11 = []
    goRight p = case burrow.hallway ! p of
      Occupied _ -> []
      Unoccupied -> p : goRight (p + 1)

allLocations :: [Location]
allLocations =
  ( do
      amphipod <- [A, B, C, D]
      InRoom amphipod <$> [Inner, Outer]
  )
    <> (InHallway <$> [1 .. 11])

pickRoomSlot :: Burrow -> Amphipod -> RoomSlot -> Tile
pickRoomSlot burrow A Inner = burrow.rooms.a.inner
pickRoomSlot burrow B Inner = burrow.rooms.b.inner
pickRoomSlot burrow C Inner = burrow.rooms.c.inner
pickRoomSlot burrow D Inner = burrow.rooms.d.inner
pickRoomSlot burrow A Outer = burrow.rooms.a.outer
pickRoomSlot burrow B Outer = burrow.rooms.b.outer
pickRoomSlot burrow C Outer = burrow.rooms.c.outer
pickRoomSlot burrow D Outer = burrow.rooms.d.outer

makeBurrow :: Rooms -> Burrow
makeBurrow = flip Burrow $ Array.array (1, 11) $ (,Unoccupied) <$> [1 .. 11]

applyStep :: Step -> Burrow -> Burrow
applyStep (MoveIntoHallway location amphi p) burrow = case location of
  InRoom A Outer -> burrow {rooms = burrow.rooms {a = burrow.rooms.a {outer = Unoccupied}}, hallway = burrow.hallway // [(p, Occupied amphi)]}
  InRoom B Outer -> burrow {rooms = burrow.rooms {b = burrow.rooms.b {outer = Unoccupied}}, hallway = burrow.hallway // [(p, Occupied amphi)]}
  InRoom C Outer -> burrow {rooms = burrow.rooms {c = burrow.rooms.c {outer = Unoccupied}}, hallway = burrow.hallway // [(p, Occupied amphi)]}
  InRoom D Outer -> burrow {rooms = burrow.rooms {d = burrow.rooms.d {outer = Unoccupied}}, hallway = burrow.hallway // [(p, Occupied amphi)]}
  InRoom A Inner -> burrow {rooms = burrow.rooms {a = burrow.rooms.a {inner = Unoccupied}}, hallway = burrow.hallway // [(p, Occupied amphi)]}
  InRoom B Inner -> burrow {rooms = burrow.rooms {b = burrow.rooms.b {inner = Unoccupied}}, hallway = burrow.hallway // [(p, Occupied amphi)]}
  InRoom C Inner -> burrow {rooms = burrow.rooms {c = burrow.rooms.c {inner = Unoccupied}}, hallway = burrow.hallway // [(p, Occupied amphi)]}
  InRoom D Inner -> burrow {rooms = burrow.rooms {d = burrow.rooms.d {inner = Unoccupied}}, hallway = burrow.hallway // [(p, Occupied amphi)]}
  InHallway srcP -> burrow {hallway = burrow.hallway // [(p, Occupied amphi), (srcP, Unoccupied)]}
applyStep (MoveIntoRoom location room slot) burrow = fillRoom room $ case location of
  InRoom A Outer -> burrow {rooms = burrow.rooms {a = burrow.rooms.a {outer = Unoccupied}}}
  InRoom B Outer -> burrow {rooms = burrow.rooms {b = burrow.rooms.b {outer = Unoccupied}}}
  InRoom C Outer -> burrow {rooms = burrow.rooms {c = burrow.rooms.c {outer = Unoccupied}}}
  InRoom D Outer -> burrow {rooms = burrow.rooms {d = burrow.rooms.d {outer = Unoccupied}}}
  InRoom A Inner -> burrow {rooms = burrow.rooms {a = burrow.rooms.a {inner = Unoccupied}}}
  InRoom B Inner -> burrow {rooms = burrow.rooms {b = burrow.rooms.b {inner = Unoccupied}}}
  InRoom C Inner -> burrow {rooms = burrow.rooms {c = burrow.rooms.c {inner = Unoccupied}}}
  InRoom D Inner -> burrow {rooms = burrow.rooms {d = burrow.rooms.d {inner = Unoccupied}}}
  InHallway p -> burrow {hallway = burrow.hallway // [(p, Unoccupied)]}
  where
    fillRoom room burrow = case room of
      A -> if slot == Outer then burrow {rooms = burrow.rooms {a = burrow.rooms.a {outer = Occupied A}}} else burrow {rooms = burrow.rooms {a = burrow.rooms.a {inner = Occupied A}}}
      B -> if slot == Outer then burrow {rooms = burrow.rooms {b = burrow.rooms.b {outer = Occupied B}}} else burrow {rooms = burrow.rooms {b = burrow.rooms.b {inner = Occupied B}}}
      C -> if slot == Outer then burrow {rooms = burrow.rooms {c = burrow.rooms.c {outer = Occupied C}}} else burrow {rooms = burrow.rooms {c = burrow.rooms.c {inner = Occupied C}}}
      D -> if slot == Outer then burrow {rooms = burrow.rooms {d = burrow.rooms.d {outer = Occupied D}}} else burrow {rooms = burrow.rooms {d = burrow.rooms.d {inner = Occupied D}}}

amphipodsAreOrganized :: Burrow -> Bool
amphipodsAreOrganized burrow =
  burrow.rooms.a == Room (Occupied A) (Occupied A)
    && burrow.rooms.b == Room (Occupied B) (Occupied B)
    && burrow.rooms.c == Room (Occupied C) (Occupied C)
    && burrow.rooms.d == Room (Occupied D) (Occupied D)

burrowOrderScore :: Burrow -> Int -- low = good
burrowOrderScore burrow = roomOrderScore A burrow.rooms.a + roomOrderScore B burrow.rooms.b + roomOrderScore C burrow.rooms.c + roomOrderScore D burrow.rooms.d
  where
    roomOrderScore :: Amphipod -> Room -> Int
    roomOrderScore amphi room = tileOrderScore amphi room.inner + tileOrderScore amphi room.outer

    tileOrderScore :: Amphipod -> Tile -> Int
    tileOrderScore room tile = case tile of
      Occupied amphi | amphi /= room -> 2
      Unoccupied -> 1
      Occupied _ -> 0

data OrdFst a b = OrdFst a b

instance (Eq a) => Eq (OrdFst a b) where
  (OrdFst a _) == (OrdFst b _) = a == b

instance (Ord a) => Ord (OrdFst a b) where
  compare (OrdFst a _) (OrdFst b _) = compare a b

findSolutions :: Burrow -> [[Step]]
findSolutions burrow = go (Heap.singleton (OrdFst 0 ([], burrow))) Map.empty
  where
    go :: Heap.Heap (OrdFst Int ([Step], Burrow)) -> Map Burrow Int -> [[Step]]
    go heap visited = case Heap.viewMin heap of
      Nothing -> []
      Just (OrdFst cost tree, remaining) -> case tree of
        (steps, burrow) | amphipodsAreOrganized burrow -> reverse steps : go remaining visited
        (steps, burrow) -> case Map.lookup burrow visited of
          Just previousCost | previousCost <= cost -> go remaining visited
          _ -> do
            let visited' = Map.insert burrow cost visited
            let fold step = Heap.insert $ OrdFst (cost + stepCost step) (step : steps, applyStep step burrow)
            go (foldr fold remaining (possibleSteps burrow)) visited'

solvePartOne :: Burrow -> Either String String
solvePartOne burrow = do
  let solutions = findSolutions burrow
  solution <- maybeToEither "No solution found!" $ safeHead solutions
  Right $ show $ sum $ map stepCost solution

partOne :: String -> Either String String
partOne = solvePartOne . makeBurrow <=< parse

partTwo :: String -> Either String String
partTwo = const $ Left "TODO"
