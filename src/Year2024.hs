module Year2024 (year) where

import Data.Map qualified as M
import Day (Day (Day), Year (Year))
import Year2024.Day1 qualified as Day1
import Year2024.Day2 qualified as Day2
import Year2024.Day3 qualified as Day3

days :: M.Map Int Day
days =
  M.fromList
    [ (1, Day Day1.partOne Day1.partTwo)
    , (2, Day Day2.partOne Day2.partTwo)
    , (3, Day Day3.partOne Day3.partTwo)
    ]

year :: Year
year = Year 2024 days
