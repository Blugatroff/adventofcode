module Year2022 (year) where

import Day (Day (Day), Year (Year))
import qualified Year2022.Day1 as Day1
import qualified Year2022.Day10 as Day10
import qualified Year2022.Day2 as Day2
import qualified Year2022.Day3 as Day3
import qualified Year2022.Day4 as Day4
import qualified Year2022.Day5 as Day5
import qualified Year2022.Day6 as Day6
import qualified Year2022.Day7 as Day7
import qualified Year2022.Day8 as Day8
import qualified Year2022.Day9 as Day9
import qualified Year2022.Day11 as Day11
import qualified Year2022.Day12 as Day12

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

year :: Year
year = Year 2022 days
