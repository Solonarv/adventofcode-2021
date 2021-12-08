module Day08 where

import AOC.Solution
import ParsingPrelude
import Util

import Data.Functor

import Data.Set (Set)
import qualified Data.Set as Set

solution :: Solution [Note] Int Int
solution = Solution
  { decodeInput = pNote `sepBy1` eol
  , solveA = defSolver
    { solve = Just . count1478
    }
  , solveB = defSolver
    { solve = Just . sum' . fmap decodeNote
    }
  , tests =
    [ "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
      :=> [(PartB, "5353")]
    , "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
      :=> [(PartB, "8394")]
    -- , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
    --   :=> [(PartB, "9781")]
    -- , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
    --   :=> [(PartB, "1197")]
    -- , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
    --   :=> [(PartB, "9361")]
    -- , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
    --   :=> [(PartB, "4873")]
    -- , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
    --   :=> [(PartB, "8418")]
    -- , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
    --   :=> [(PartB, "4548")]
    -- , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
    --   :=> [(PartB, "1625")]
    -- , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
    --   :=> [(PartB, "8717")]
    -- , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    --   :=> [(PartB, "4315")]
    , unlines
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
      ] :=> [(PartA, "26"), (PartB, "61229")]
    ]
  }

data Note = Note [Set Wire] [Set Wire]
  deriving (Eq, Ord, Show)

data Wire = A | B | C | D | E | F | G
  deriving (Eq, Ord, Show, Enum)

pPattern :: Parser (Set Wire)
pPattern = Set.fromList <$> many pWire
  where
    pWire = asum
      [ A <$ char 'a', B <$ char 'b', C <$ char 'c'
      , D <$ char 'd', E <$ char 'e', F <$ char 'f'
      , G <$ char 'g'
      ]

pNote :: Parser Note
pNote = Note
  <$> patterns
  <* char '|' <* space
  <*> patterns
  where patterns = pPattern `sepBy1` char ' '

to1478 :: Set Wire -> Maybe Int
to1478 pat = case Set.size pat of
  2 -> Just 1
  4 -> Just 4
  3 -> Just 7
  7 -> Just 8
  _ -> Nothing

count1478 :: [Note] -> Int
count1478 = sum' . fmap note1478
  where
    note1478 (Note _ pats) = countHits (isJust . to1478) pats

decodeNote :: Note -> Int
decodeNote (Note pats digs) = fromDigits 10 $ decodeDigits pats digs

decodeDigits :: [Set Wire] -> [Set Wire] -> [Int]
decodeDigits pats digs = digs <&> \dig -> case Set.size dig of
  1 -> error "invalid digit with only one segment"
  2 -> 1
  4 -> 4
  3 -> 7
  7 -> 8
  5 -> case Set.size $ Set.intersection dig pat4 of
    2 -> 2
    3 -> case Set.size $ Set.intersection dig pat1 of
      1 -> 5
      2 -> 3
      _ -> error "invalid five-segment digit that doesn't intersect once or twice with 1"
    _ -> error "invalid five-segment digit that isn't 2, 3, or 5"
  6 -> case Set.size $ Set.intersection dig pat4 of
    3 -> case Set.size $ Set.intersection dig pat1 of
      1 -> 6
      2 -> 0
      _ -> error "invalid six-segment digit that doesn't intersect once or twice with 1"
    4 -> 9
    _ -> error "invalid six-segment digit that doesn't intersect three or four times with 4"
  _ -> error "impossible digit with 0 or >7 segments"
    
  where
    Just pat1 = findFirstWith (guarding $ (==2) . Set.size) pats
    Just pat4 = findFirstWith (guarding $ (==4) . Set.size) pats
    -- we don't actually need this
    -- Just pat7 = findFirstWith (guarding $ (==3) . Set.size) pats