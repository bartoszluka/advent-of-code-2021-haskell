module Main where

import Day01
import Day02
import Day03
import Day05 (done5part1, howManyOverlapAtleast2)
import Day06 (done6part2)
import Inputs

-- main :: IO ()
-- main = do
--   contents <- getContents
--   let list = choose readMaybeInt . lines $ contents
--    in print $ howManyIncreased2 list

-- main :: IO ()
-- main = do
--   contents <- getContents
--   let list = lines contents
--    in print $ lifeSupportRating list

main :: IO ()
main = do
  print $ done6part2 day6list