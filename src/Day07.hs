module Day07 where

import Control.Monad (liftM2)
import Data.List (genericLength, minimumBy)
import GHC.Float (rationalToDouble)

howMuchFuel :: Num c => c -> [c] -> c
howMuchFuel to = sum . map (abs . subtract to)

listMax :: Ord a => [a] -> a
listMax = foldl1 (\acc curr -> if curr > acc then curr else acc)

listMin :: Ord a => [a] -> a
listMin = foldl1 (\acc curr -> if curr < acc then curr else acc)

leastFuelNeeded1 :: [Integer] -> Integer
leastFuelNeeded1 initialList =
  let allPositions = [listMin initialList .. listMax initialList]
      distances = map (`howMuchFuel` initialList) allPositions
   in listMin distances

leastFuelNeeded2 :: (Ord a, Enum a, Fractional a) => [a] -> a
leastFuelNeeded2 initialList =
  let allPositions = [listMin initialList .. listMax initialList]
      distances = map (howMuchFuel2 initialList) allPositions
   in listMin distances

howMuchFuel2 :: Fractional c => [c] -> c -> c
howMuchFuel2 list to = sum . map (distance to) $ list
  where
    distance from to =
      fuelCost $ abs (from - to)
    fuelCost n = (1 + n) * n / 2

done7part1 = leastFuelNeeded1

done7part2 = leastFuelNeeded2 . map toRational