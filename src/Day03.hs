module Day03 where

import Data.List (partition, transpose)
import Extra

-- start binary

data BinaryDigit = Zero | One
  deriving (Eq, Enum)

type BinaryNumber = [BinaryDigit]

instance Show BinaryDigit where
  show Zero = "0"
  show One = "1"

invert :: BinaryDigit -> BinaryDigit
invert Zero = One
invert One = Zero

fromChar :: Char -> Maybe BinaryDigit
fromChar '0' = Just Zero
fromChar '1' = Just One
fromChar _ = Nothing

toInt :: BinaryDigit -> Int
toInt Zero = 0
toInt One = 1

binToDecimal :: BinaryNumber -> Int
binToDecimal = foldl (\acc curr -> (2 * acc) + toInt curr) 0

fromString :: String -> Maybe BinaryNumber
fromString =
  foldr (\curr acc -> fromChar curr >>= addToMaybeList acc) (Just [])
  where
    addToMaybeList (Just list) item = Just (item : list)
    addToMaybeList Nothing _ = Nothing

-- end binary

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

lifeSupportRating codes =
  co2Scrubber 0 numbers * oxygenGeneratorRating 0 numbers
  where
    numbers = map (map toBinaryDigits) codes