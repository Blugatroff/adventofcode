module Year2023 (year) where

import Data.Map qualified as M
import Day (Day (Day), Year (Year))
import Year2023.Day5 qualified as Day5

days :: M.Map Int Day
days =
  M.fromList
    [ (5, Day Day5.partOne Day5.partTwo)
    ]

year :: Year
year = Year 2023 days
