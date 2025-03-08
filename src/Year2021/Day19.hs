module Year2021.Day19 (partOne, partTwo) where

import MeLude
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Pos3
import Util (applyN, dedup, findMap, readInt, safeHead, safeMaximum, safeTail, split, splitSeq, trim, tuplePermutations)

data Scanner = Scanner {id :: Int, beacons :: [Pos3 Int]}

parseScanner :: String -> Either String Scanner
parseScanner str = do
  let ls = filter (not . null) $ map (trim isSpace) $ lines str
  header <- maybeToEither ("failed to parse scanner: " <> str) $ safeHead ls
  let digits = filter isDigit header
  let error = "failed to parse scanner id of scanner: " <> str
  id <- mapLeft (const error) $ readInt digits

  tail <- maybeToEither ("failed to parse scanner: " <> str) $ safeTail ls
  beacons <- traverse parsePos tail
  Right $ Scanner {id, beacons}

parsePos :: String -> Either String (Pos3 Int)
parsePos str = do
  splits <- traverse readInt $ split ',' $ trim isSpace str
  case splits of
    [x, y, z] -> Right $ Pos3 x y z
    _ -> Left $ "failed to parse pos: " <> str

parse :: String -> Either String [Scanner]
parse = traverse parseScanner . splitSeq "\n\n"

type Orientation = Pos3 Int -> Pos3 Int

rotateAroundX :: Pos3 Int -> Pos3 Int
rotateAroundX (Pos3 x y z) = Pos3 x (-z) y
rotateAroundY :: Pos3 Int -> Pos3 Int
rotateAroundY (Pos3 x y z) = Pos3 (-z) y x
rotateAroundZ :: Pos3 Int -> Pos3 Int
rotateAroundZ (Pos3 x y z) = Pos3 (-y) x z

allOrientations :: [Orientation]
allOrientations =
  concat
    [ [0 .. 3] <&> \n -> applyN n rotateAroundZ . applyN 0 rotateAroundX,
      [0 .. 3] <&> \n -> applyN n rotateAroundY . applyN 1 rotateAroundX,
      [0 .. 3] <&> \n -> applyN n rotateAroundZ . applyN 2 rotateAroundX,
      [0 .. 3] <&> \n -> applyN n rotateAroundY . applyN 3 rotateAroundX,
      [0 .. 3] <&> \n -> applyN n rotateAroundX . applyN 1 rotateAroundY,
      [0 .. 3] <&> \n -> applyN n rotateAroundX . applyN 3 rotateAroundY
    ]

findOverlapAtSameOrientation :: [Pos3 Int] -> [Pos3 Int] -> Maybe (Pos3 Int)
findOverlapAtSameOrientation beaconsA beaconsB =
  flip findMap beaconsB $ \beaconB -> findMap (findMatches beaconB) beaconsA
  where
    findMatches beaconB beaconA = do
      let offset = beaconA - beaconB
      let pred p = p `elem` beaconsA
      let nMatches = length $ filter (pred . (+) offset) beaconsB
      if nMatches == 12 then Just offset else Nothing

findOverlap :: [Pos3 Int] -> [Pos3 Int] -> Maybe ([Pos3 Int], Pos3 Int)
findOverlap beaconsA beaconsB = do
  flip findMap allOrientations $ \orientation -> do
    let reorientedBeaconsB = map orientation beaconsB
    offset <- findOverlapAtSameOrientation beaconsA reorientedBeaconsB
    Just (reorientedBeaconsB, offset)

data Network = Network {scanner :: Scanner, overlaps :: [Overlap]}

data Overlap = Overlap {offset :: Pos3 Int, network :: Network}

scannersInNetwork :: Network -> [(Pos3 Int, Scanner)]
scannersInNetwork = go 0
  where
    go pos (Network {scanner, overlaps}) =
      (pos, scanner) : (overlaps >>= \overlap -> go (pos + overlap.offset) overlap.network)

buildNetwork :: Scanner -> State [Scanner] Network
buildNetwork start = do
  State.modify $ filter (\scanner -> scanner.id /= start.id)
  scanners :: [Scanner] <- State.get
  overlaps <- fmap catMaybes $ for scanners $ \scanner -> do
    alreadyConnected <- State.gets $ all (\s -> s.id /= scanner.id)
    if alreadyConnected
      then do
        pure Nothing
      else do
        case findOverlap start.beacons scanner.beacons of
          Nothing -> pure Nothing
          Just (reorientedBeaconsB, offset) -> do
            let reorientedScanner = scanner {beacons = reorientedBeaconsB}
            nextNetwork <- buildNetwork reorientedScanner
            pure $ Just $ Overlap {offset, network = nextNetwork}
  pure $ Network {scanner = start, overlaps}

solve :: [Scanner] -> Either String [(Pos3 Int, Scanner)]
solve scanners = do
  first <- maybeToEither "need at least one scanner" $ safeHead scanners
  let network = State.evalState (buildNetwork first) scanners
  pure $ scannersInNetwork network

solvePartOne :: [(Pos3 Int, Scanner)] -> Int
solvePartOne scanners = do
  let beacons = scanners >>= \(pos, scanner) -> map (+ pos) scanner.beacons
  let uniqueBeacons = dedup beacons
  length uniqueBeacons

solvePartTwo :: [(Pos3 Int, Scanner)] -> Either String Int
solvePartTwo scanners = do
  let distances = map (uncurry manhattan) $ tuplePermutations $ map fst scanners
  maybeToEither "need at least one sensor" $ safeMaximum distances

partOne :: String -> Either String String
partOne input = parse input >>= solve <&> show . solvePartOne

partTwo :: String -> Either String String
partTwo input = parse input >>= solve <&> show . solvePartTwo
