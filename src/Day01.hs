module Day01 where

import Extra (pairs, triplets)

howManyIncreased :: [Int] -> Int
howManyIncreased =
  length . filter (uncurry (<)) . pairs

howManyIncreased2 :: [Int] -> Int
howManyIncreased2 =
  howManyIncreased
    . map multiply
    . triplets
  where
    multiply (a, b, c) = a + b + c
