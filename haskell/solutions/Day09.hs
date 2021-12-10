module Day09 where

import AOC.Solution
import ParsingPrelude
import Util
import Grid2D

import Data.List (sortOn)
import Data.Ord (Down(..))

import Control.Lens
import Data.Set (Set)
import qualified Data.Set as Set

solution :: Solution (Grid2D Int) Int Int
solution = Solution
  { decodeInput = fromLines <$> some (singleDigit 10) `sepBy1` space1
  , solveA = defSolver
    { solve = Just . sum' . fmap ((+1) . snd) . itoListOf lowPoints
    }
  , solveB = defSolver
    { solve = Just . product . fmap Set.size . nLargest 3 . basins 9
    }
  , tests =
    [ unlines
      [ "2199943210"
      , "3987894921"
      , "9856789892"
      , "8767896789"
      , "9899965678"
      ] :=> [(PartA, "15"), (PartB, "1134")]
    ]
  }

lowPoints :: Ord a => IndexedTraversal' (Int, Int) (Grid2D a) a
lowPoints f grid = (itraversed . ifiltered isLowPoint) f grid
  where
    isLowPoint (x,y) a = all (>a) (adjacentsNeumann x y grid)

basin :: Ord a => a -> Grid2D a -> Int -> Int -> a -> Set (Int, Int)
basin s grid x y a = if a >= s
  then Set.empty
  else Set.singleton (x,y) <> Set.unions
    [ basin s grid x' y' a'
    | (x', y') <- [(x-1,y), (x+1,y), (x,y-1), (x, y+1)]
    , Just a' <- [grid ^? gridPoint x' y']
    , a' > a
    ]

basins :: Ord a => a -> Grid2D a -> [Set (Int, Int)]
basins s grid = itoListOf lowPoints grid
  <&> \((x,y),a) -> basin s grid x y a

nLargest :: Ord a => Int -> [Set a] -> [Set a]
nLargest n = take n . sortOn (Down . Set.size)

