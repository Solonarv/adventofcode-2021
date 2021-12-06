module Day06 where

import AOC.Solution
import ParsingPrelude
import Util

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector

_MAX_AGE :: Int
_MAX_AGE = 8

_BREED_COOLDOWN :: Int
_BREED_COOLDOWN = 6

solution :: Solution (Vector Int) Int Int
solution = Solution
  { decodeInput = collect <$> decimal `sepBy1` string ","
  , solveA = defSolver
    { solve = Just . Vector.sum . funcpow 80 step
    }
  , solveB = defSolver
    { solve = Just . Vector.sum . funcpow 256 step
    }
  , tests =
    [ "3,4,3,1,2"
      :=> [(PartA, "5934"), (PartB, "26984457539")]
    ]
  }
  where
    collect xs = Vector.create do
      vec <- MVector.replicate (_MAX_AGE + 1) 0
      for_ xs \a -> MVector.modify vec (+1) a
      pure vec

step :: Vector Int -> Vector Int
step ages = let
    breeding = Vector.head ages
  in Vector.create do
    ages' <- MVector.new (_MAX_AGE + 1)
    for_ [1 .. _MAX_AGE] \i -> do
      MVector.write ages' (i-1) (ages Vector.! i)
    MVector.modify ages' (+ breeding) _BREED_COOLDOWN
    MVector.write ages' _MAX_AGE breeding
    pure ages'