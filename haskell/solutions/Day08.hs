module Day08 where

import AOC.Solution
import ParsingPrelude
import Util

import Data.Bool
import Data.Functor

import Data.Vector (Vector)
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Unboxed as UVector

solution :: Solution [Note] Int Int
solution = Solution
  { decodeInput = pNote `sepBy1` eol
  , solveA = defSolver
    { solve = Just . count1478
    }
  , solveB = defSolver
  , tests =
    [ unlines
      [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
      , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
      , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
      , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
      , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
      , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
      , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
      , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
      , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
      , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
      ] :=> [(PartA, "26")]
    ]
  }

data Note = Note (Vector Pattern) (Vector Pattern)
  deriving (Eq, Ord, Show)

data Pattern = Pattern { patSignals :: UVector.Vector Bool }
  deriving (Eq, Ord, Show)

wireLabels :: UVector.Vector Char
wireLabels = Vector.fromList "abcdefg"

showPattern :: Pattern -> String
showPattern = Vector.toList . Vector.mapMaybe combine . Vector.zip wireLabels . patSignals
  where combine (c,p) = c <$ guard p

pPattern :: Parser Pattern
pPattern = many (oneOf "abcdefg") <&> Pattern . \labs ->
  Vector.map (\c -> c `elem` labs) wireLabels

pNote :: Parser Note
pNote = Note
  <$> patterns
  <* char '|' <* space
  <*> patterns
  where patterns = Vector.fromList <$> pPattern `sepBy1` char ' '

onSignals :: Pattern -> Int
onSignals = Vector.sum . Vector.map (bool 0 1) . patSignals

to1478 :: Pattern -> Maybe Int
to1478 pat = case onSignals pat of
  2 -> Just 1
  4 -> Just 4
  3 -> Just 7
  7 -> Just 8
  _ -> Nothing

count1478 :: [Note] -> Int
count1478 = sum' . fmap note1478
  where
    note1478 (Note _ pats) = Vector.length $ Vector.filter (isJust . to1478) pats