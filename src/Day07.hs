{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Day07 (part1, part2) where

import Control.Monad (liftM2)
import Data.List (genericLength, minimumBy)
import Extra (average, median)
import GHC.Float (int2Double, rationalToDouble)

howMuchFuel :: Num c => (t1 -> t2 -> c) -> [t1] -> t2 -> c
howMuchFuel distance list to = sum . map (\from -> distance from to) $ list

leastFuelNeeded1 :: [Integer] -> Integer
leastFuelNeeded1 =
    calc (howMuchFuel distance) ($) median
  where
    calc = flip liftM2
    distance from to = abs (from - to)

leastFuelNeeded2 :: Integral a => [a] -> a
leastFuelNeeded2 initialList =
    min (fuelNeeded fl) (fuelNeeded ceil)
  where
    potential = average . map toRational $ initialList
    (fl, ceil) = (floor potential, ceiling potential)
    fuelNeeded = howMuchFuel distance initialList
    distance from to = fuelCost $ abs (from - to)
    fuelCost n = ((1 + n) * n) `div` 2

part1 :: [Integer] -> Integer
part1 = leastFuelNeeded1

part2 :: [Integer] -> Integer
part2 = leastFuelNeeded2