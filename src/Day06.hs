module Day06 (part1, part1parsing, part2) where

import Control.Monad (liftM2)
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
nextDay = updateSixth . ixmap (0, 8) (decMod 9)
  where
    decMod maxValue n = (n + 1) `mod` maxValue
    updateSixth arr = arr // [(6, arr ! 6 + arr ! 8)]

toSimulation :: [Int] -> SimulationDay
toSimulation =
    accumArray increment 0 (0, 8) . map duplicate
  where
    duplicate a = (a, a)
    increment n _ = n + 1

fishCount :: SimulationDay -> Int
fishCount = sum . elems

simulateFish :: SimulationDay -> [SimulationDay]
simulateFish currentDay = currentDay : simulateFish (nextDay currentDay)

done :: Int -> [Int] -> Int
done day = fishCount . nth day . simulateFish . toSimulation

nth :: Int -> [a] -> a
nth = flip (!!)

done6parsing :: Text -> Maybe Int
done6parsing = fmap part1 . readMaybe . toString . surroundWithBrackets
  where
    surroundWithBrackets :: Text -> Text
    surroundWithBrackets txt = "[" <> txt <> "]"

part1parsing :: Text -> Maybe Int
part1parsing = done6parsing

part1 :: [Int] -> Int
part1 = done 80

part2 :: [Int] -> Int
part2 = done 256