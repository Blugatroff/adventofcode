module Days
  ( days,
    Day,
    partOne,
    partTwo,
    Part,
  )
where

import qualified Days.Day1 as Day1
import qualified Days.Day10 as Day10
import qualified Days.Day11 as Day11
import qualified Days.Day12 as Day12
import qualified Days.Day2 as Day2
import qualified Days.Day3 as Day3
import qualified Days.Day4 as Day4
import qualified Days.Day5 as Day5
import qualified Days.Day6 as Day6
import qualified Days.Day7 as Day7
import qualified Days.Day8 as Day8
import qualified Days.Day9 as Day9

type Part = String -> Either String String

data Day = Day
  { partOne :: !Part,
    partTwo :: !Part
  }

days :: [Day]
days =
  [ Day Day1.partOne Day1.partTwo,
    Day Day2.partOne Day2.partTwo,
    Day Day3.partOne Day3.partTwo,
    Day Day4.partOne Day4.partTwo,
    Day Day5.partOne Day5.partTwo,
    Day Day6.partOne Day6.partTwo,
    Day Day7.partOne Day7.partTwo,
    Day Day8.partOne Day8.partTwo,
    Day Day9.partOne Day9.partTwo,
    Day Day10.partOne Day10.partTwo,
    Day Day11.partOne Day11.partTwo,
    Day Day12.partOne Day12.partTwo
  ]
