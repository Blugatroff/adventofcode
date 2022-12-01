module Day
  ( Day (Day, partOne, partTwo),
    Part,
    Year (Year, days, name),
  )
where

type Part = String -> Either String String

data Day = Day
  { partOne :: !Part,
    partTwo :: !Part
  }

data Year = Year
  { name :: !Int,
    days :: ![Day]
  }

instance Show Year where
  show = show . name
