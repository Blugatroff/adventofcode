module Year2022 (year) where

import Day (Day (Day), Year (Year))
import qualified Year2022.Day1 as Day1
import qualified Year2022.Day2 as Day2
import qualified Year2022.Day3 as Day3
import qualified Year2022.Day4 as Day4

days :: [Day]
days =
  [ Day Day1.partOne Day1.partTwo,
    Day Day2.partOne Day2.partTwo,
    Day Day3.partOne Day3.partTwo,
    Day Day4.partOne Day4.partTwo
  ]

year :: Year
year = Year 2022 days
