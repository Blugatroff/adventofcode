module Day
  ( Day (Day, partOne, partTwo),
    Part,
    Year (Year, days, name),
  )
where

import Data.Map qualified as M

type Part = String -> Either String String

data Day = Day
  { partOne :: !Part,
    partTwo :: !Part
  }

data Year = Year
  { name :: !Int,
    days :: !(M.Map Int Day)
  }

instance Show Year where
  show = show . name
