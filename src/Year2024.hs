module Year2024 (year) where

import Data.Map qualified as M
import Day (Day (Day), Year (Year))
import Year2024.Day1 qualified as Day1
import Year2024.Day2 qualified as Day2
import Year2024.Day3 qualified as Day3
import Year2024.Day4 qualified as Day4
import Year2024.Day5 qualified as Day5
import Year2024.Day6 qualified as Day6
import Year2024.Day7 qualified as Day7

days :: M.Map Int Day
days =
  M.fromList
    [ (1, Day Day1.partOne Day1.partTwo)
    , (2, Day Day2.partOne Day2.partTwo)
    , (3, Day Day3.partOne Day3.partTwo)
    , (4, Day Day4.partOne Day4.partTwo)
    , (5, Day Day5.partOne Day5.partTwo)
    , (6, Day Day6.partOne Day6.partTwo)
    , (7, Day Day7.partOne Day7.partTwo)
    ]

year :: Year
year = Year 2024 days
