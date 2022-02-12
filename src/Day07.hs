module Day07 where

import Control.Monad (liftM2)
import Data.List (genericLength, minimumBy)
import Extra (average, median)
import GHC.Float (int2Double, rationalToDouble)

howMuchFuel :: Num a => [a] -> a -> a
howMuchFuel list to = sum . map (abs . subtract to) $ list

leastFuelNeeded1 :: [Integer] -> Integer
leastFuelNeeded1 =
    calc howMuchFuel ($) median
  where
    calc = flip liftM2

leastFuelNeeded2 :: Integral a => [a] -> a
leastFuelNeeded2 initialList =
    min (fuelNeeded fl) (fuelNeeded ceil)
  where
    potential = average . map toRational $ initialList
    (fl, ceil) = (floor potential, ceiling potential)
    fuelNeeded = howMuchFuel2 initialList

howMuchFuel2 :: Integral a => [a] -> a -> a
howMuchFuel2 list to = sum . map (distance to) $ list
  where
    distance from to =
        fuelCost $ abs (from - to)
    fuelCost n = ((1 + n) * n) `div` 2

part1 :: [Integer] -> Integer
part1 = leastFuelNeeded1

part2 :: [Integer] -> Integer
part2 = leastFuelNeeded2