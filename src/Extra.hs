{-# LANGUAGE LambdaCase #-}

module Extra where

import Data.List (partition)
import Text.Read (readMaybe)

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

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

itemOf :: Int -> [a] -> Maybe a
index `itemOf` list =
  let len = length list
   in if abs index > len
        then Nothing
        else Just (list !! (index `mod` len))

at = flip itemOf

at1 list index = (index - 1) `itemOf` list

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
