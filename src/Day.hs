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

instance Show Day where
  show _ = "Day"

data Year = Year
  { name :: !Int,
    days :: !(M.Map Int Day)
  } deriving Show

