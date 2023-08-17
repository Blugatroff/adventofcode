module Year2022 (year) where

import Data.Map qualified as M
import Day (Day (Day), Year (Year))
import Year2022.Day1 qualified as Day1
import Year2022.Day10 qualified as Day10
import Year2022.Day11 qualified as Day11
import Year2022.Day12 qualified as Day12
import Year2022.Day13 qualified as Day13
import Year2022.Day14 qualified as Day14
import Year2022.Day15 qualified as Day15
import Year2022.Day16 qualified as Day16
import Year2022.Day17 qualified as Day17
import Year2022.Day18 qualified as Day18
import Year2022.Day19 qualified as Day19
import Year2022.Day20 qualified as Day20
import Year2022.Day2 qualified as Day2
import Year2022.Day3 qualified as Day3
import Year2022.Day4 qualified as Day4
import Year2022.Day5 qualified as Day5
import Year2022.Day6 qualified as Day6
import Year2022.Day7 qualified as Day7
import Year2022.Day8 qualified as Day8
import Year2022.Day9 qualified as Day9

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
    , (13, Day Day13.partOne Day13.partTwo)
    , (14, Day Day14.partOne Day14.partTwo)
    , (15, Day Day15.partOne Day15.partTwo)
    , (16, Day Day16.partOne Day16.partTwo)
    , (17, Day Day17.partOne Day17.partTwo)
    , (18, Day Day18.partOne Day18.partTwo)
    , (19, Day Day19.partOne Day19.partTwo)
    , (20, Day Day20.partOne Day20.partTwo)
    ]

year :: Year
year = Year 2022 days
