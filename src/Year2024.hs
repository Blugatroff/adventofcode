module Year2024 (year) where

import MeLude
import Data.Map qualified as M
import Day (Day (Day), Year (Year))
import Year2024.Day1 qualified as Day1
import Year2024.Day2 qualified as Day2
import Year2024.Day3 qualified as Day3
import Year2024.Day4 qualified as Day4
import Year2024.Day5 qualified as Day5
import Year2024.Day6 qualified as Day6
import Year2024.Day7 qualified as Day7
import Year2024.Day8 qualified as Day8
import Year2024.Day9 qualified as Day9
import Year2024.Day10 qualified as Day10
import Year2024.Day11 qualified as Day11
import Year2024.Day12 qualified as Day12

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
    , (8, Day Day8.partOne Day8.partTwo)
    , (9, Day Day9.partOne Day9.partTwo)
    , (10, Day Day10.partOne Day10.partTwo)
    , (11, Day Day11.partOne Day11.partTwo)
    , (12, Day Day12.partOne Day12.partTwo)
    ]

year :: Year
year = Year 2024 days
