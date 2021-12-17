module Day06 where

import Control.Monad (liftM2)
import Parsing (parseInput, runParse)

data SimulationDay = SimulationDay
  { counter0 :: Int,
    counter1 :: Int,
    counter2 :: Int,
    counter3 :: Int,
    counter4 :: Int,
    counter5 :: Int,
    counter6 :: Int,
    counter7 :: Int,
    counter8 :: Int
  }
  deriving (Show, Eq)

nextDay :: SimulationDay -> SimulationDay
nextDay sd =
  SimulationDay
    { counter0 = counter1 sd,
      counter1 = counter2 sd,
      counter2 = counter3 sd,
      counter3 = counter4 sd,
      counter4 = counter5 sd,
      counter5 = counter6 sd,
      counter6 = counter7 sd + counter0 sd,
      counter7 = counter8 sd,
      counter8 = counter0 sd
    }

fromList :: (Num a, Eq a) => [a] -> SimulationDay
fromList list =
  let c n = length . filter (== n) $ list
   in SimulationDay (c 0) (c 1) (c 2) (c 3) (c 4) (c 5) (c 6) (c 7) (c 8)

fishCount :: SimulationDay -> Int
fishCount sd =
  counter0 sd
    + counter1 sd
    + counter2 sd
    + counter3 sd
    + counter4 sd
    + counter5 sd
    + counter6 sd
    + counter7 sd
    + counter8 sd

simulateFish :: SimulationDay -> [SimulationDay]
simulateFish currentDay = currentDay : simulateFish (nextDay currentDay)

done6 :: (Num a, Eq a) => Int -> [a] -> Int
done6 day = fishCount . nth day . simulateFish . fromList

done6parsing :: String -> Maybe Int
done6parsing = fmap done6part1 . toMaybe . runParse parseInput
  where
    toMaybe (Left _) = Nothing
    toMaybe (Right x) = Just x

nth :: Int -> [a] -> a
nth = flip (!!)

done6part1 :: [Int] -> Int
done6part1 = done6 80

done6part2 :: [Int] -> Int
done6part2 = done6 256