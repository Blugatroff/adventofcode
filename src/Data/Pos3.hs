module Data.Pos3 (Pos3(..), x, y, z, manhattan) where

import MeLude

data Pos3 a = Pos3 a a a

instance Eq a => Eq (Pos3 a) where
  (Pos3 x1 y1 z1) == (Pos3 x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

instance Ord a => Ord (Pos3 a) where
  compare (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = case compare x1 x2 of
    LT -> LT
    GT -> GT
    EQ -> case compare y1 y2 of
      LT -> LT
      GT -> GT
      EQ -> compare z1 z2

instance Show a => Show (Pos3 a) where
  show (Pos3 x y z) = "(Pos3 " <> show x <> " " <> show y <> " " <> show z <> ")"

x :: Pos3 a -> a
x (Pos3 x _ _) = x

y :: Pos3 a -> a
y (Pos3 _ y _) = y

z :: Pos3 a -> a
z (Pos3 _ _ z) = z

instance Num a => Num (Pos3 a) where
  (Pos3 x1 y1 z1) + (Pos3 x2 y2 z2) = Pos3 (x1 + x2) (y1 + y2) (z1 + z2)
  (Pos3 x1 y1 z1) - (Pos3 x2 y2 z2) = Pos3 (x1 - x2) (y1 - y2) (z1 - z2)
  (Pos3 x1 y1 z1) * (Pos3 x2 y2 z2) = Pos3 (x1 * x2) (y1 * y2) (z1 * z2)
  abs (Pos3 x y z) = Pos3 (abs x) (abs y) (abs z)
  fromInteger n = Pos3 (fromInteger n) 0 0
  signum (Pos3 x y z) = Pos3 (signum x) (signum y) (signum z)

manhattan :: Num a => Pos3 a -> Pos3 a -> a
manhattan (Pos3 x1 y1 z1) (Pos3 x2 y2 z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

