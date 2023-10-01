module Data.Pos (Pos(..), x, y) where

data Pos = Pos {y :: Int, x :: Int}
  deriving (Show, Eq)

instance Ord Pos where
  compare (Pos y1 x1) (Pos y2 x2) = compare (y1, x1) (y2, x2)

instance Num Pos where
  (Pos y1 x1) + (Pos y2 x2) = Pos (y1 + y2) (x1 + x2)
  (Pos y1 x1) * (Pos y2 x2) = Pos (y1 * y2) (x1 * x2)
  abs (Pos y x) = Pos (abs y) (abs x)
  negate (Pos y x) = Pos (-y) (-x)
  signum (Pos y x) = Pos (signum y) (signum x)
  fromInteger i = Pos (fromInteger i) (fromInteger i)

