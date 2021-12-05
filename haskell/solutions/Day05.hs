module Day05 where

import AOC.Solution
import ParsingPrelude
import Util

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Linear.V2

solution :: Solution [(V2 Int, V2 Int)] Int Int
solution = Solution
  { decodeInput = line `sepBy1` space1
  , solveA = defSolver
    { solve = Just . countHits (>1) . toGridAA
    }
  , solveB = defSolver
    { solve = Just . countHits (>1) . toGrid
    }
  , tests =
    [ "0,9 -> 5,9 8,0 -> 0,8 9,4 -> 3,4 2,2 -> 2,1 7,0 -> 7,4 6,4 -> 2,0 0,9 -> 2,9 3,4 -> 1,4 0,0 -> 8,8 5,5 -> 8,2"
      :=> [(PartA, "5"), (PartB, "12")]
    ]
  }
  where
    line = (,) <$> point <* string " -> " <*> point
    point = V2 <$> decimal <* string "," <*> decimal

toGridAA :: [(V2 Int, V2 Int)] -> Map (V2 Int) Int
toGridAA = Map.fromListWith (+) . fmap (,1) . concatMap axisAlignedlinePoints

axisAlignedlinePoints :: (V2 Int, V2 Int) -> [V2 Int]
axisAlignedlinePoints (V2 x1 y1, V2 x2 y2)
  | x1 == x2 = V2 x1 <$> range y1 y2
  | y1 == y2 = flip V2 y1 <$> range x1 x2
  | otherwise = []
    

toGrid :: [(V2 Int, V2 Int)] -> Map (V2 Int) Int
toGrid = Map.fromListWith (+) . fmap (,1) . concatMap linePoints

linePoints :: (V2 Int, V2 Int) -> [V2 Int]
linePoints (V2 x1 y1, V2 x2 y2)
  | x1 == x2 = V2 x1 <$> range y1 y2
  | y1 == y2 = flip V2 y1 <$> range x1 x2
  | otherwise = zipWith V2 (range x1 x2) (range y1 y2)

range :: Int -> Int -> [Int]
range a b
      | a == b = [a]
      | a > b = enumFromThenTo a (a-1) b
      | otherwise = enumFromTo a b