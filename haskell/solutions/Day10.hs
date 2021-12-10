module Day10 where

import AOC.Solution
import ParsingPrelude
import Util

import Data.Either
import Data.List

solution :: Solution [String] Int Int
solution = Solution
  { decodeInput = some (oneOf "[]{}()<>") `sepBy1` eol
  , solveA = defSolver
    { solve = Just . sum'  . fmap pointsError . lefts . fmap parseLine
    }
  , solveB = defSolver
    { solve = median . fmap scoreCompletion . rights . fmap parseLine
    }
  , tests =
    [ unlines
      [ "[({(<(())[]>[[{[]{<()<>>"
      , "[(()[<>])]({[<{<<[]>>("
      , "{([(<{}[<>[]}>{[]{[(<()>"
      , "(((({<>}<{<{<>}{[]{[]{}"
      , "[[<[([]))<([[{}[[()]]]"
      , "[{[{({}]{}}([{[{{{}}([]"
      , "{<[[]]>}<{[{[{[]{()[[[]"
      , "[<(<(<(<{}))><([]([]()"
      , "<{([([[(<>()){}]>(<<{{"
      , "<{([{{}}[<[[[<>{}]]]>[]]"
      ] :=> [(PartA, "26397"), (PartB, "288957")]
    ]
  }

parseLine :: String -> Either Char String
parseLine = go []
  where
    go stk (c:cs)
      | isOpener c
      = go (c:stk) cs
      | o:rest <- stk, isCloser c
      = if c == closerOf o
        then go rest cs
        else Left c
      | otherwise = go stk cs
    go stk [] = Right stk

isOpener, isCloser :: Char -> Bool
isOpener c = c `elem` "<{[("
isCloser c = c `elem` ">}])"

closerOf :: Char -> Char
closerOf = \case
  '<' -> '>'
  '{' -> '}'
  '[' -> ']'
  '(' -> ')'
  c -> c

pointsError :: Char -> Int
pointsError = \case
  '>' -> 25137
  '}' -> 1197
  ']' -> 57
  ')' -> 3
  _ -> 0

scoreCompletion :: String -> Int
scoreCompletion = go 0
  where
    go a [] = a
    go !a (c:cs) = case c of
      '(' -> go (a*5+1) cs
      '[' -> go (a*5+2) cs
      '{' -> go (a*5+3) cs
      '<' -> go (a*5+4) cs
      _ -> go a cs

median :: Ord a => [a] -> Maybe a
median = safely \xs -> sort xs !! (length xs `div` 2)