module Year2022 (year) where

import Day (Day (Day), Year (Year))
import qualified Year2022.Day1 as Day1
import qualified Year2022.Day2 as Day2
import qualified Year2022.Day3 as Day3
import qualified Year2022.Day4 as Day4
import qualified Year2022.Day5 as Day5
import qualified Year2022.Day6 as Day6

days :: [Day]
days =
  [ Day Day1.partOne Day1.partTwo,
    Day Day2.partOne Day2.partTwo,
    Day Day3.partOne Day3.partTwo,
    Day Day4.partOne Day4.partTwo,
    Day Day5.partOne Day5.partTwo,
    Day Day6.partOne Day6.partTwo
  ]

year :: Year
year = Year 2022 days
