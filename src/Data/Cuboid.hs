module Data.Cuboid (Cuboid(..), intersects, intersection, contains, containsPoint, points, volume) where

import Data.Pos3 (Pos3(..))
import Data.Range qualified as Range
import Data.Range (Range)
import Data.Functor ((<&>))

data Cuboid a = Cuboid (Range a) (Range a) (Range a)

instance Show a => Show (Cuboid a) where
  show (Cuboid x y z) = "(Cuboid (" <> show x <> ") (" <> show y <> ") (" <> show z <> "))"

intersects :: Ord a => Cuboid a -> Cuboid a -> Bool
intersects (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = Range.intersects x1 x2 && Range.intersects y1 y2 && Range.intersects z1 z2

intersection :: Ord a => Cuboid a -> Cuboid a -> Maybe (Cuboid a)
intersection (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = do
  x <- Range.intersection x1 x2
  y <- Range.intersection y1 y2
  z <- Range.intersection z1 z2
  Just $ Cuboid x y z

contains :: Ord a => Cuboid a -> Cuboid a -> Bool
contains (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = x1 `Range.contains` x2 && y1 `Range.contains` y2 && z1 `Range.contains` z2

containsPoint :: Ord a => Cuboid a -> Pos3 a -> Bool
containsPoint (Cuboid rx ry rz) (Pos3 x y z) = Range.inRange rx x && Range.inRange ry y && Range.inRange rz z

points :: Cuboid Int -> [Pos3 Int]
points (Cuboid x y z) = Range.points x >>= \x -> Range.points y >>= \y -> Range.points z <&> \z -> Pos3 x y z

volume :: Num a => Cuboid a -> a
volume (Cuboid x y z) = Range.size x * Range.size y * Range.size z
