module Day03 where

import Data.List (partition, transpose)
import Extra (pairMap)

mostCommonDigit :: [Int] -> Int
mostCommonDigit number =
  if ones >= zeros then 1 else 0
  where
    (ones, zeros) = pairMap length . partition (1 ==) $ number

toBinaryDigits :: Char -> Int
toBinaryDigits '1' = 1
toBinaryDigits _ = 0

toDecimal :: [Int] -> Int
toDecimal = foldl (\acc curr -> 2 * acc + curr) 0

invertDigit :: Int -> Int
invertDigit i = if i == 0 then 1 else 0

invertDigits :: [Int] -> [Int]
invertDigits = map invertDigit

powerConsumed :: [String] -> Int
powerConsumed codes =
  gamma * epsilon
  where
    digits = map mostCommonDigit . transpose . map (map toBinaryDigits) $ codes
    gamma = toDecimal digits
    epsilon = toDecimal . invertDigits $ digits

-- unsafe
mostCommonDigitAtPosition :: Int -> [[Int]] -> Int
mostCommonDigitAtPosition pos = mostCommonDigit . (!! pos) . transpose

-- unsafe
filterDigitAtPosition :: Int -> Int -> [[Int]] -> [[Int]]
filterDigitAtPosition pos mostCommon = filter (\number -> (number !! pos) == mostCommon)

oxygenGeneratorRating :: Int -> [[Int]] -> Int
oxygenGeneratorRating _ [number] = toDecimal number
oxygenGeneratorRating _ [] = 0
oxygenGeneratorRating pos rest = oxygenGeneratorRating (pos + 1) . filterDigitAtPosition pos digit $ rest
  where
    digit = mostCommonDigitAtPosition pos rest

co2Scrubber :: Int -> [[Int]] -> Int
co2Scrubber _ [number] = toDecimal number
co2Scrubber _ [] = 0
co2Scrubber pos rest = co2Scrubber (pos + 1) . filterDigitAtPosition pos (invertDigit digit) $ rest
  where
    digit = mostCommonDigitAtPosition pos rest

lifeSupportRating :: [String] -> Int
lifeSupportRating codes =
  co2Scrubber 0 numbers * oxygenGeneratorRating 0 numbers
  where
    numbers = map (map toBinaryDigits) codes