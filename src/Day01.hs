module Day01 (part1, part2) where

import Extra (pairs, triplets)

howManyIncreased :: [Int] -> Int
howManyIncreased = length . filter (uncurry (<)) . pairs

howManyIncreased2 :: [Int] -> Int
howManyIncreased2 = howManyIncreased . map sumUp . triplets
  where
    sumUp (a, b, c) = a + b + c

part1 :: [Int] -> Int
part1 = howManyIncreased

part2 :: [Int] -> Int
part2 = howManyIncreased2
