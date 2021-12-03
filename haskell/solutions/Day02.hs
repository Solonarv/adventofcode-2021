module Day02 where

import AOC.Solution
import ParsingPrelude
import Util

import Linear.V2

solution :: Solution [V2 Int] Int Int
solution = Solution
  { decodeInput = move `sepBy1` space1
  , solveA = defSolver
    { solve = Just . product . sum 
    }
  , solveB = defSolver
    { solve = Just . product . fst .  foldl' step (V2 0 0, 0)

    }
  , tests =
    [ "forward 5 down 5 forward 8 up 3 down 8 forward 2" :=> [(PartA, "150"), (PartB, "900")]
    ]
  }

move :: Parser (V2 Int)
move = dir <* space1 <*> decimal
  where
    dir = asum
      [ (V2 1 0 *) <$ string "forward"
      , (V2 0 1 *) <$ string "down"
      , (V2 0 (-1) *) <$ string "up"
      ]
    
step :: (V2 Int, Int) -> V2 Int -> (V2 Int, Int)
step (pos, aim) (V2 dx da) = (pos + V2 dx (aim*dx), aim + da)