module Day06 (part1, part1parsing, part2) where

import Data.Array.Unboxed (
    UArray,
    accumArray,
    elems,
    ixmap,
    (!),
    (//),
 )
import Relude.Unsafe ((!!))

type SimulationDay = UArray Int Int

nextDay :: SimulationDay -> SimulationDay
nextDay = ixmap (0, 8) (decMod 9) .> updateSixth
  where
    decMod maxValue n = (n + 1) `mod` maxValue
    updateSixth arr = arr // [(6, arr ! 6 + arr ! 8)]

toSimulation :: [Int] -> SimulationDay
toSimulation =
    map duplicate .> accumArray increment 0 (0, 8)
  where
    duplicate a = (a, a)
    increment n _ = n + 1

fishCount :: SimulationDay -> Int
fishCount = elems .> sum

simulateFish :: SimulationDay -> [SimulationDay]
simulateFish currentDay = currentDay : simulateFish (nextDay currentDay)

done :: Int -> [Int] -> Int
done day = toSimulation .> simulateFish .> nth day .> fishCount

nth :: Int -> [a] -> a
nth = flip (!!)

done6parsing :: Text -> Maybe Int
done6parsing = surroundWithBrackets .> toString .> readMaybe .> fmap part1
  where
    surroundWithBrackets :: Text -> Text
    surroundWithBrackets txt = "[" <> txt <> "]"

part1parsing :: Text -> Maybe Int
part1parsing = done6parsing

part1 :: [Int] -> Int
part1 = done 80

part2 :: [Int] -> Int
part2 = done 256