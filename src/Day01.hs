module Day01 where

import Extra

-- 1602
howManyIncreased :: [Int] -> Int
howManyIncreased =
  length . filter (uncurry (<)) . pairs

-- 1633
howManyIncreased2 :: [Int] -> Int
howManyIncreased2 =
  howManyIncreased
    . map multiply
    . triplets
  where
    multiply (a, b, c) = a + b + c
