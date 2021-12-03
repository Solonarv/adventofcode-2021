module Day03 where

import AOC.Solution
import ParsingPrelude
import Util

import Debug.Trace

import Data.Bits
import Data.Bool
import Data.Vector (Vector)
import qualified Data.Vector as Vector

solution :: Solution (Int, [Vector Int]) Int Int
solution = Solution
  { decodeInput = smoothen <$> bitvec `sepBy1` space1
  , solveA = defSolver
    { solve = Just . mult . extractGE . summarize . snd
    }
  , solveB = defSolver
    { solve = \inp -> mult <$> liftA2 (,) (findOxy inp) (findCO2 inp)
    }
  , tests =
    [ "00100 11110 10110 10111 10101 01111 00111 11100 10000 11001 00010 01010"
      :=> [(PartA, "198"), (PartB, "230")]
    ]
  }
  where mult (x, y) = x*y

bitvec :: Parser (Vector Int)
bitvec = Vector.fromList <$> (many $ asum [(-1) <$ string "0", 1 <$ string "1"])

smoothen :: [Vector Int] -> (Int, [Vector Int])
smoothen vs = (ml, Vector.reverse . lengthen <$> vs)
  where
    ml = maximum (length <$> vs)
    lengthen v = v <> Vector.replicate (-1) (ml - length v)

toBitVec :: Int -> Vector Int
toBitVec x = traceShowId $ Vector.generate l (bool (-1) 1 . testBit x)
  where l = finiteBitSize x - countLeadingZeros x - 1

fromBitsVec :: Vector Int -> Int
fromBitsVec = Vector.ifoldl' g 0
  where
    g x i cnt
      | cnt > 0 = setBit x i 
      | cnt == 0 = error ("ambiguous count for bit " ++ show i)
      | otherwise = x

summarize :: Foldable t => t (Vector Int) -> Vector Int
summarize = foldl1 (Vector.zipWith (+))

extractGE :: Vector Int -> (Int, Int)
extractGE vec = (fromBitsVec vec, fromBitsVec (negate <$> vec))

findOxy :: (Int, [Vector Int]) -> Maybe Int
findOxy (ml, vecs) = fromBitsVec <$> findBit (>= 0) (ml-1) vecs

findCO2 :: (Int, [Vector Int]) -> Maybe Int
findCO2 (ml, vecs) = fromBitsVec <$> findBit (< 0) (ml-1) vecs

findBit :: (Int -> Bool) -> Int -> [Vector Int] -> Maybe (Vector Int)
findBit crit = go
  where
    go _ [v] = Just v
    go _ [] = Nothing
    go (-1) _ = Nothing
    go i vecs = let
      mostCommon = sum' [ v Vector.! i | v <- vecs ]
      desiredBit = if crit mostCommon then 1 else -1
      in go (i-1) [ v | v <- vecs, v Vector.! i == desiredBit ]