module Extra where

import Control.Monad (liftM2)
import qualified Data.HashMap.Strict as HMap
import Data.List (partition)
import qualified Data.Map as Map
import Data.Text (splitOn)
import Relude.Extra (bimapBoth)

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (a, b) = (f a, f b)

-- [(first, second), (second, third), ...]
pairs :: [a] -> [(a, a)]
pairs = \case
    [] -> []
    full@(_ : xs) -> zip full xs

-- [(first, second, third), (second, third, fourth), ...]
triplets :: [a] -> [(a, a, a)]
triplets = \case
    [] -> []
    [_] -> []
    full@(_ : noFirst@(_ : rest)) -> zip3 full noFirst rest

choose :: (a -> Maybe b) -> [a] -> [b]
choose chooser =
    foldr
        ( \x acc -> case chooser x of
            Nothing -> acc
            Just y -> y : acc
        )
        []

groupCount :: Eq a => [a] -> [(a, Int)]
groupCount list = go list []
  where
    go :: Eq a => [a] -> [(a, Int)] -> [(a, Int)]
    go [] accumulated = accumulated
    go (x : xs) accumulated =
        let (equal, rest) = partition (x ==) xs
         in go rest ((x, length equal + 1) : accumulated)

countEachHash :: (Eq k, Hashable k) => [k] -> [(k, Int)]
countEachHash items = zip items (repeat 1) |> HMap.fromListWith (+) |> HMap.toList

countEachOrd :: (Ord k) => [k] -> [(k, Int)]
countEachOrd items = zip items (repeat 1) |> Map.fromListWith (+) |> Map.toList

toMaybe :: Either err ok -> Maybe ok
toMaybe (Right x) = Just x
toMaybe (Left _) = Nothing

createRange :: (Ord a, Enum a) => a -> a -> [a]
createRange x y =
    if x < y
        then [x .. y]
        else reverse [y .. x]

zipToLonger :: [a] -> [b] -> [(a, b)]
zipToLonger xs ys =
    if length xs < length ys
        then zip (cycle xs) ys
        else zip xs (cycle ys)

median :: (Ord a) => [a] -> a
median list = selectKth half list
  where
    half :: Integer = ceiling <| (genericLength list / 2 :: Double)

selectKth :: (Integral i, Ord a, Show i) => i -> [a] -> a
selectKth k [] = error <| "k = " <> show k <> " is greater than the length"
selectKth k full@(x : _)
    | k <= lenLess = selectKth k less
    | k <= lenLess + lenEqual = x
    | otherwise = selectKth (k - lenLess - lenEqual) greater
  where
    (less, equal, greater) = partitionCompare x full
    (lenLess, lenEqual) = bimapBoth len (less, equal)
    len = genericLength

biggerLength :: [a] -> [a] -> [a]
biggerLength xs ys = if length xs > length ys then xs else ys

partitionCompare :: (Foldable t, Ord a) => a -> t a -> ([a], [a], [a])
partitionCompare toCompare =
    foldr
        ( \curr (smaller, equal, bigger) -> case curr `compare` toCompare of
            LT -> (curr : smaller, equal, bigger)
            EQ -> (smaller, curr : equal, bigger)
            GT -> (smaller, equal, curr : bigger)
        )
        ([], [], [])

count' :: (a -> Bool) -> [a] -> Int
count' predicate list = length (filter predicate list)

count :: Foldable l => (a -> Bool) -> l a -> Int
count predicate = foldMap (predicate .> toInt .> Sum) .> getSum
  where
    toInt True = 1
    toInt False = 0

between :: Ord a => a -> (a, a) -> Bool
n `between` (lower, upper) = lower <= n && n <= upper

average :: Fractional a => [a] -> a
average = calc sum (/) len
  where
    calc = flip liftM2
    len = genericLength

printInGhci :: Maybe [Text] -> IO ()
printInGhci = fmap (unlines .> toString) .> fromMaybe "" .> putStrLn

splitInTwo :: Text -> Text -> Maybe (Text, Text)
splitInTwo sep text = case splitOn sep text of
    [x, y] -> Just (x, y)
    _ -> Nothing

readMaybeText :: (Read a) => Text -> Maybe a
readMaybeText = toString .> readMaybe

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)
