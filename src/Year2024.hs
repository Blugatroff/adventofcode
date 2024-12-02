module Year2024 (year) where

import Data.Map qualified as M
import Day (Day (Day), Year (Year))
import Year2024.Day1 qualified as Day1

days :: M.Map Int Day
days =
  M.fromList
    [ (1, Day Day1.partOne Day1.partTwo)
    ]

year :: Year
year = Year 2024 days
