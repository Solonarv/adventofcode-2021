module Day07 where

import AOC.Solution
import ParsingPrelude
-- import Util

-- import Debug.Trace
import Control.Monad.ST

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import qualified Data.Vector.Algorithms.Radix as Radix

solution :: Solution (Vector Int) Int Int
solution = Solution
  { decodeInput = Vector.fromList <$> decimal `sepBy1` string ","
  , solveA = defSolver
    { solve = Just . linFuelRequired
    }
  , solveB = defSolver
    { solve = Just . squareFuelRequiredBruteForce
    }
  , tests =
    [ "16,1,2,0,4,2,7,1,2,14"
      :=> [(PartA, "37"), (PartB, "168")]
    ]
  }

-- | Value of the error term minimized by the median.
linFuelRequired :: Vector Int -> Int
linFuelRequired xs = Vector.sum (Vector.map dist xs)
  where
    med = median xs
    dist x = abs (x-med)

-- | Finds the median of a vector of integers.
-- If the input length is even, returns the higher of the values.
median :: Vector Int -> Int
median vec = runST do
  mvec <- Vector.thaw vec
  Radix.sort mvec
  MVector.read mvec (MVector.length mvec `div` 2)

-- | Value of the error term minimized by the average.
squareFuelRequired :: Vector Int -> Int
squareFuelRequired xs = Vector.sum (Vector.map dist xs)
  where
    avg = average xs
    dist x = let !d = abs (x-avg) in d*(d+1) `div` 2

-- what the fuck
squareFuelRequiredBruteForce :: Vector Int -> Int
squareFuelRequiredBruteForce xs = Vector.minimum $ Vector.generate (hi-lo+1) requiredFuel
  where
    lo = Vector.minimum xs
    hi = Vector.maximum xs
    requiredFuel ((+lo) -> mid) = Vector.sum (Vector.map dist xs)
      where dist x = let !d = abs (x-mid) in d*(d+1) `div` 2


-- | Returns the average, rounded.
average :: Vector Int -> Int
average vec = Vector.sum vec `divRoundly` Vector.length vec
  where
    divRoundly p q = round (fromIntegral p / fromIntegral q :: Double)