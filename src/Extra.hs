{-# LANGUAGE LambdaCase #-}

module Extra where

import Control.Monad (liftM2)
import Data.List (partition)
import Text.Read (readMaybe)

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (a, b) = (f a, f b)

-- [(first, second), (second, third), ...]
pairs :: [a] -> [(a, a)]
pairs = \case
    [] -> []
    all@(x : xs) -> zip all xs

-- [(first, second, third), (second, third, fourth), ...]
triplets :: [a] -> [(a, a, a)]
triplets = \case
    [] -> []
    [_] -> []
    all@(_ : noFirst@(_ : rest)) -> zip3 all noFirst rest

choose :: (a -> Maybe b) -> [a] -> [b]
choose chooser =
    foldr
        ( \x acc -> case chooser x of
            Nothing -> acc
            Just x -> x : acc
        )
        []

at1 list index = list !!? (index - 1)

groupCount :: Eq a => [a] -> [(a, Int)]
groupCount list = go list []
  where
    go :: Eq a => [a] -> [(a, Int)] -> [(a, Int)]
    go [] accumulated = accumulated
    go (x : xs) accumulated =
        let (equal, rest) = partition (x ==) xs
         in go rest ((x, length equal + 1) : accumulated)

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
    half = ceiling $ genericLength list / 2

selectKth :: (Integral i, Ord a, Show i) => i -> [a] -> a
selectKth k [] = error $ "k = " <> show k <> " is greater than the length"
selectKth k all@(x : xs)
    | k <= lenLess = selectKth k less
    | k <= lenLess + lenEqual = x
    | otherwise = selectKth (k - lenLess - lenEqual) greater
  where
    (less, equal, greater) = partitionCompare x all
    [lenLess, lenEqual] = len <$> [less, equal]
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

count :: (a -> Bool) -> [a] -> Int
count predicate = getSum . foldMap (Sum . toInt . predicate)
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
