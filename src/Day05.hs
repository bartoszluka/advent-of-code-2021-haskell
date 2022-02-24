module Day05 (part1, part2) where

import qualified Data.Map as Map
import Extra (choose, createRange, splitInTwo, zipToLonger)

type Point = (Int, Int)

type Vector = (Point, Point)

pointParser :: Text -> Maybe Point
pointParser input = do
    (x, y) <- splitInTwo "," input
    x' <- readMaybe <| toString x
    y' <- readMaybe <| toString y
    return (x', y')

lineParser :: Text -> Maybe Vector
lineParser input = do
    (p1, p2) <- splitInTwo " -> " input
    p1' <- pointParser p1
    p2' <- pointParser p2
    return (p1', p2')

toVectors :: Text -> [Vector]
toVectors = lines .> choose lineParser

getPoints :: Vector -> [Point]
getPoints ((x1, y1), (x2, y2)) =
    zipToLonger xs ys
  where
    xs = createRange x1 x2
    ys = createRange y1 y2

toMap :: (Ord k, Num a) => [k] -> Map k a
toMap list = Map.fromListWith (+) zipped
  where
    zipped = zip list (repeat 1)

howManyOverlapAtleast :: (Ord a, Num a, Foldable f) => a -> f Vector -> Int
howManyOverlapAtleast n vectors =
    let pseudoMap = foldMap getPoints vectors
        grouped = toMap pseudoMap
     in Map.size (Map.filter (>= n) grouped)

howManyOverlapAtleast2 :: [Vector] -> Int
howManyOverlapAtleast2 = howManyOverlapAtleast (2 :: Int)

part1 :: Text -> Int
part1 = done5 <| filter horizontalOrVertical
  where
    horizontalOrVertical ((x1, y1), (x2, y2)) = x2 == x1 || y1 == y2

part2 :: Text -> Int
part2 = done5 id

done5 :: ([Vector] -> [Vector]) -> Text -> Int
done5 process = toVectors .> process .> howManyOverlapAtleast2
