module Year2024.Day12 (partOne, partTwo) where

import MeLude
import Util

import Control.Monad.State (State, evalState, gets, modify')
import Data.Array.Base qualified as Array
import Data.Monoid (Sum (Sum), getSum)
import Data.Pos (Pos (..))
import Data.Set qualified as Set
import Direction (Direction (..), allDirections)

type GardenMap = UArray Pos Char

parse :: String -> Either String GardenMap
parse = parseGrid Right

type Region = Set Pos

findRegions :: GardenMap -> [Region]
findRegions garden = runST $ do
  let neighbours pos = filter (inRange (bounds garden)) [pos + Pos dir.x dir.y | dir <- allDirections]

  flooded <- Array.thawSTUArray $ Array.amap (const False) garden
  let mark p = Array.writeArray flooded p True
  let marked = Array.readArray flooded

  let flood pos = do
        mark pos
        let plant = garden ! pos
        let visitNeighbour = \case
              neighbour | garden ! neighbour == plant -> do
                whenMonoidM (not <$> marked neighbour) $ flood neighbour
              _ -> pure Set.empty
        Set.insert pos . Set.unions <$> for (neighbours pos) visitNeighbour
  fmap catMaybes $ for (Array.indices garden) $ \pos -> do
    whenMonoidM (not <$> marked pos) $ pure <$> flood pos

sides :: Region -> Int
sides region = flip evalState Set.empty $ do
  let flood :: Pos -> Direction -> State (Set (Pos, Direction)) ()
      flood pos side = do
        modify' $ Set.insert (pos, side)

        let neighbours :: [Pos]
            neighbours = do
              dir <- case side of
                DirLeft -> [DirUp, DirDown]
                DirRight -> [DirUp, DirDown]
                DirDown -> [DirLeft, DirRight]
                DirUp -> [DirLeft, DirRight]
              let neighbour = pos + Pos dir.x dir.y
              guard $ Set.member neighbour region
              guard $ not $ Set.member (neighbour + Pos side.x side.y) region
              pure neighbour

            visitNeighbour :: Pos -> State (Set (Pos, Direction)) ()
            visitNeighbour neighbour = do
              alreadyFlooded <- gets $ Set.member (neighbour, side)
              unless alreadyFlooded $ flood neighbour side
        for_ neighbours visitNeighbour

  fmap getSum $ mconcatForM (Set.elems region) $ \pos -> do
    mconcatForM allDirections $ \side -> do
      alreadyCounted <- gets $ Set.member (pos, side)
      let sideIsExposed = not $ Set.member (pos + Pos side.x side.y) region
      whenMonoid (not alreadyCounted && sideIsExposed) $ do
        flood pos side
        pure $ Sum (1 :: Int)

perimeter :: Region -> Int
perimeter region = length $ do
  pos <- Set.elems region
  dir <- allDirections
  let neighbour = pos + Pos dir.x dir.y
  guard $ not $ Set.member neighbour region

area :: Region -> Int
area = Set.size

partOne :: String -> Either String String
partOne input = do
  garden <- parse input
  let regions = findRegions garden
  Right $ show $ sum $ do
    region <- regions
    pure $ perimeter region * area region

partTwo :: String -> Either String String
partTwo input = do
  garden <- parse input
  let regions = findRegions garden
  Right $ show $ sum $ do
    region <- regions
    pure $ sides region * area region
