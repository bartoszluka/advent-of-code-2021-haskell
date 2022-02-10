module Day06 (part1, part2) where

import Control.Monad (liftM2)
import Data.Array (Array)
import Data.Array.IArray (
    Array,
    accumArray,
    elems,
    ixmap,
    (!),
    (//),
 )
import Parsing (parseInput, runParse)

type SimulationDay = Array Int Int

nextDay :: SimulationDay -> SimulationDay
nextDay = updateSixth . ixmap (0, 8) (decMod 9)
  where
    decMod maxValue n = (n + 1) `mod` maxValue
    updateSixth arr = arr // [(6, arr ! 6 + arr ! 8)]

fromList :: [Int] -> SimulationDay
fromList =
    accumArray increment 0 (0, 8) . map duplicate
  where
    duplicate a = (a, a)
    increment n _ = n + 1

fishCount :: SimulationDay -> Int
fishCount = sum . elems

simulateFish :: SimulationDay -> [SimulationDay]
simulateFish currentDay = currentDay : simulateFish (nextDay currentDay)

done :: Int -> [Int] -> Int
done day = fishCount . nth day . simulateFish . fromList

done6parsing :: String -> Maybe Int
done6parsing = fmap part1 . toMaybe . runParse parseInput
  where
    toMaybe (Left _) = Nothing
    toMaybe (Right x) = Just x

nth :: Int -> [a] -> a
nth = flip (!!)

part1 :: [Int] -> Int
part1 = done 80

part2 :: [Int] -> Int
part2 = done 256