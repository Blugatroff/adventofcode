module Data.Pos (Pos (..), divPos) where

import MeLude

import Foreign.Ptr (castPtr)
import Foreign.Storable

data Pos = Pos {x :: Int, y :: Int}
  deriving (Eq, Ix)

instance Storable Pos where
  sizeOf _ = sizeOf (undefined :: Pos).x + sizeOf (undefined :: Pos).y
  alignment = sizeOf
  peek posPtr = do
    let ptr = castPtr posPtr
    Pos <$> peekElemOff ptr 0 <*> peekElemOff ptr 1
  poke posPtr (Pos x y) = do
    let ptr = castPtr posPtr
    pokeElemOff ptr 0 x
    pokeElemOff ptr 1 y

instance Show Pos where
  show (Pos x y) = show (x, y)

instance Ord Pos where
  compare (Pos x1 y1) (Pos x2 y2) = compare (y1, x1) (y2, x2)

instance Num Pos where
  (Pos x1 y1) + (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)
  (Pos x1 y1) * (Pos x2 y2) = Pos (x1 * x2) (y1 * y2)
  abs (Pos x y) = Pos (abs x) (abs y)
  negate (Pos x y) = Pos (-x) (-y)
  signum (Pos x y) = Pos (signum x) (signum y)
  fromInteger i = Pos (fromInteger i) 0

divPos :: Pos -> Int -> Pos
divPos (Pos x y) d = Pos (x `div` d) (y `div` d)
