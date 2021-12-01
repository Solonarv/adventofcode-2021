module Day01 where

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [Int] Int Int
solution = Solution
  { decodeInput = decimal `sepBy` space1
  , solveA = defSolver
  { solve = Just . countIncreases
  }
  , solveB = defSolver
  { solve = Just . countIncreases . slidingWindow
  }
  , tests =
    [ "199 200 208 210 200 207 240 269 260 263" :=> [(PartA, "7"), (PartB, "5")]
    ]
  }

countIncreases :: [Int] -> Int
countIncreases xs = countHits id $ zipWith (>) (drop 1 xs) xs

slidingWindow :: [Int] -> [Int]
slidingWindow xs = zipWith3 add xs (drop 1 xs) (drop 2 xs)
  where add x y z = x + y + z