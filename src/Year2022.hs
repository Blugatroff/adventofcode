module Year2022 (year) where

import Day (Day (Day), Year (Year))
import qualified Year2022.Day1 as Day1

days :: [Day]
days =
  [ Day Day1.partOne Day1.partTwo
  ]

year :: Year
year = Year 2022 days
