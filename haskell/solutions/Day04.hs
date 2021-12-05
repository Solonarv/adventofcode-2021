module Day04 where

import AOC.Solution
import ParsingPrelude
import Util
import Grid2D

import Data.List (partition)

solution :: Solution ([Int], [Grid2D Int]) Int Int
solution = Solution
  { decodeInput = liftA2 (,) numbers grids
  , solveA = defSolver
    { solve = fmap score . firstWinningGrid . uncurry successiveGrids . augment
    }
  , solveB = defSolver
    { solve = fmap score . lastWinningGrid . uncurry successiveGrids . augment
    }
  , tests =
    [ unlines
      [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
      , ""
      , "22 13 17 11  0"
      , " 8  2 23  4 24"
      , "21  9 14 16  7"
      , " 6 10  3 18  5"
      , " 1 12 20 15 19"
      , ""
      , " 3 15  0  2 22"
      , " 9 18 13 17  5"
      , "19  8  7 25 23"
      , "20 11 10 24  4"
      , "14 21 16 12  6"
      , ""
      , "14 21 17 24  4"
      , "10 16 15  9 19"
      , "18  8 23 26 20"
      , "22 11 13  6  5"
      , " 2  0 12  3  7 "
      ] :=> [(PartA, "4512"), (PartB, "1924")]
  ]
  }
  where
    numbers = decimal `sepBy1` string "," <* eol <* eol
    grids = grid `sepBy1` eol
    grid = fromLines <$> gridLine `sepEndBy1` eol
    gridLine = noeol *> decimal `sepBy1` noeol
    augment = fmap (fmap (fmap Just))

successiveGrids :: [Int] -> [Grid2D (Maybe Int)] -> [(Int, [Grid2D (Maybe Int)])]
successiveGrids [] _ = []
successiveGrids (x:xs) gs = (x, gs') : successiveGrids xs gs'
  where
    gs' = fmap keep <$> gs
    keep (Just y) | x == y = Nothing
    keep y = y

isWinning :: Grid2D (Maybe Int) -> Bool
isWinning grid = winsRow || winsCol
  where
    winsRow = any wins (rows grid)
    winsCol = any wins (cols grid)
    wins = all isNothing

winningGrids :: [(Int, [Grid2D (Maybe Int)])] -> [(Int, [Grid2D (Maybe Int)])]
winningGrids = (fmap . fmap) (filter isWinning)

firstWinningGrid :: [(Int, [Grid2D (Maybe Int)])] -> Maybe (Int, Grid2D (Maybe Int))
firstWinningGrid = findFirstWith (\case (x, [g]) -> Just (x, g); _ -> Nothing) . winningGrids

score :: (Int, Grid2D (Maybe Int)) -> Int
score (x, g) = x * sum' (fromMaybe 0 <$> g)

lastWinningGrid :: [(Int, [Grid2D (Maybe Int)])] -> Maybe (Int, Grid2D (Maybe Int))
lastWinningGrid ggs = case partition (not . all isWinning . snd) ggs of
  (safeLast -> Just (_, prev), (x, next):_) -> (x,) <$>
    listToMaybe [ n | (p, n) <- zip prev next, not (isWinning p), isWinning n ]
  _ -> Nothing