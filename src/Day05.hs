module Day05 where

import qualified Data.HashMap as HM
import Extra (choose, readMaybeInt, toMaybe)
import Inputs (day5)
import Parsing (integer, runParse)
import Text.Parsec (char, eof, string)
import Text.Parsec.String (Parser)

type Point = (Int, Int)

type Vector = (Point, Point)

pointParser :: Parser Point
pointParser = do
  x <- integer
  char ','
  y <- integer
  return (x, y)

lineParser :: Parser Vector
lineParser = do
  p1 <- pointParser
  string " -> "
  p2 <- pointParser
  eof
  return (p1, p2)

toVectors :: [String] -> [Vector]
toVectors = choose (toMaybe . runParse lineParser)

getPoints :: Vector -> [Point]
getPoints ((x1, y1), (x2, y2)) =
  zipToLonger xs ys
  where
    xs = createRange x1 x2
    ys = createRange y1 y2
    zipToLonger xs ys =
      if length xs < length ys
        then zip (cycle xs) ys
        else zip xs (cycle ys)
    createRange x y =
      if x < y
        then [x .. y]
        else reverse [y .. x]

toMap list = HM.fromListWith (+) zipped
  where
    zipped = zip list (repeat 1)

howManyOverlapAtleast :: (Ord a, Num a, Foldable t) => a -> t Vector -> Int
howManyOverlapAtleast n vectors =
  let pseudoMap = foldMap getPoints vectors
      grouped = toMap pseudoMap
   in HM.size (HM.filter (>= n) grouped)

howManyOverlapAtleast2 :: [Vector] -> Int
howManyOverlapAtleast2 = howManyOverlapAtleast 2

done5part1 :: [String] -> Int
done5part1 = done5 $ filter horizontalOrVertical
  where
    horizontalOrVertical ((x1, y1), (x2, y2)) = x2 == x1 || y1 == y2

done5part2 :: [String] -> Int
done5part2 = done5 id

done5 :: ([Vector] -> [Vector]) -> [String] -> Int
done5 process = howManyOverlapAtleast2 . process . toVectors
  where
    horizontalOrVertical ((x1, y1), (x2, y2)) = x2 == x1 || y1 == y2
