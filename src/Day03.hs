module Day03 (part1, part2) where

import Control.Monad (liftM2)
import Data.List (partition)
import Relude.Extra (bimapBoth)

mostCommonDigit :: [Int] -> Int
mostCommonDigit number =
    if ones >= zeros then 1 else 0
  where
    (ones, zeros) = partition (1 ==) .> bimapBoth length <| number

toBinaryDigits :: Char -> Int
toBinaryDigits '1' = 1
toBinaryDigits _ = 0

toDecimal :: [Int] -> Int
toDecimal = foldl' (\acc curr -> 2 * acc + curr) 0

invertDigit :: Int -> Int
invertDigit i = if i == 0 then 1 else 0

invertDigits :: [Int] -> [Int]
invertDigits = map invertDigit

powerConsumed :: [String] -> Int
powerConsumed = calc gamma (*) epsilon
  where
    digits = map (map toBinaryDigits) .> transpose .> map mostCommonDigit
    gamma = digits .> toDecimal
    epsilon = digits .> invertDigits .> toDecimal
    calc = flip liftM2

mostCommonDigitAtPosition :: Int -> [[Int]] -> Maybe Int
mostCommonDigitAtPosition pos =
    transpose
        .> (!!? pos)
        .>> mostCommonDigit

filterDigitAtPosition :: Int -> Int -> [[Int]] -> [[Int]]
filterDigitAtPosition pos mostCommon = filter isMostCommon
  where
    isMostCommon = lookAt pos .>> isEqual mostCommon .> defaultTo False
    isEqual = (==)
    lookAt = flip (!!?)
    defaultTo = fromMaybe

oxygenGeneratorRating :: Int -> [[Int]] -> Maybe Int
oxygenGeneratorRating _ [number] = toDecimal number |> Just
oxygenGeneratorRating _ [] = Nothing
oxygenGeneratorRating pos rest = do
    digit <- mostCommonDigitAtPosition pos rest
    rest
        |> filterDigitAtPosition pos digit
        |> oxygenGeneratorRating (pos + 1)

co2Scrubber :: Int -> [[Int]] -> Maybe Int
co2Scrubber _ [] = Nothing
co2Scrubber _ [number] = toDecimal number |> Just
co2Scrubber pos rest =
    do
        digit <- mostCommonDigitAtPosition pos rest
        rest
            |> filterDigitAtPosition pos (invertDigit digit)
            |> co2Scrubber (pos + 1)

lifeSupportRating :: [String] -> Maybe Int
lifeSupportRating codes = calc co2 (*) oxygen
  where
    co2 = co2Scrubber 0 numbers
    oxygen = oxygenGeneratorRating 0 numbers
    numbers = map (map toBinaryDigits) codes
    calc = flip liftM2

part1 :: [String] -> Int
part1 = powerConsumed

part2 :: [String] -> Maybe Int
part2 = lifeSupportRating
