{-# LANGUAGE DeriveFunctor #-}

module Day11 where

import Data.Array.IArray (
    IArray,
    Ix,
    amap,
    array,
    assocs,
    bounds,
    elems,
    inRange,
    range,
    (!),
    (//),
 )
import Data.Array.Unboxed (
    Array,
 )
import Data.Foldable (maximum)
import Data.Text (chunksOf, concat)
import Extra (count)
import Text.Show (show)
import Prelude hiding (show)

type Index = (Int, Int)

type Matrix a = Array Index a

data Visited a = Visited a | NotVisited a
    deriving (Eq, Show, Functor)

extract :: Visited a -> a
extract (Visited x) = x
extract (NotVisited x) = x

unVisit :: Visited a -> Visited a
unVisit (NotVisited a) = NotVisited a
unVisit (Visited a) = NotVisited a

data Octopus = Flashed | NotFlashed Int
    deriving (Eq)

isFlashed :: Octopus -> Bool
isFlashed Flashed = True
isFlashed (NotFlashed _) = False

instance Show Octopus where
    show Flashed = "*"
    show (NotFlashed n) = show n

miniInput :: Text
miniInput =
    unlines
        [ "5483143223"
        , "2745854711"
        , "5264556173"
        , "6141336146"
        , "6357385478"
        , "4167524645"
        , "2176841721"
        , "6882881134"
        , "4846848554"
        , "5283751526"
        ]

autoArray :: [(Index, Octopus)] -> Matrix Octopus
autoArray list = array ((1, 1), (i, j)) list
  where
    i = list |> map (fst .> fst) |> maximum
    j = list |> map (fst .> snd) |> maximum

toMatrix :: Text -> Maybe (Matrix Octopus)
toMatrix =
    lines .> traverse oneLine -- maybe cośtam
        .>> indexed
        .>> concatMap addIndexes
        .>> map (mapSnd NotFlashed)
        .>> autoArray
  where
    oneLine :: Text -> Maybe [Index]
    oneLine = toString .> traverse (one .> readMaybe) .> fmap indexed
    addIndexes (index, list) = map (\(innerIdx, val) -> ((index, innerIdx), val)) list
    indexed = zip [1 ..]
    mapSnd function (x, y) = (x, function y)

safeIndex :: (Ix i, IArray ix el) => i -> ix i el -> Maybe el
safeIndex idx arr =
    if inRange (bounds arr) idx
        then Just <| arr ! idx
        else Nothing

genNeighbors :: Index -> Matrix a -> [Index]
genNeighbors index@(i, j) matrix =
    case safeIndex index matrix of
        Nothing -> []
        Just _ ->
            [ (i - 1, j - 1)
            , (i - 1, j)
            , (i - 1, j + 1)
            , (i, j - 1)
            , (i, j + 1)
            , (i + 1, j - 1)
            , (i + 1, j)
            , (i + 1, j + 1)
            ]

increaseEnergy :: Octopus -> Octopus
increaseEnergy Flashed = Flashed
increaseEnergy (NotFlashed n)
    | n < 9 =
        NotFlashed (n + 1)
    | otherwise = Flashed

prettyPrintMatrix :: Show a => Matrix a -> Text
prettyPrintMatrix matrix = matrix |> assocs |> map (snd .> show .> toText) |> Data.Text.concat |> chunksOf size |> unlines
  where
    size = bounds matrix |> snd |> fst

printNewLines :: Text -> IO ()
printNewLines = toString .> putStrLn

debug x = trace (show x) x

flashAndUpdate :: Index -> Matrix (Visited Octopus) -> Matrix (Visited Octopus)
flashAndUpdate index octoMatrix = case safeIndex index octoMatrix of
    Just (NotVisited octopus) -> case increaseEnergy octopus of
        Flashed -> foldr flashAndUpdate (updateMatrixWith Flashed) neighbors
        NotFlashed n ->
            if n >= 10
                then flashAndUpdate index (updateMatrixWith Flashed)
                else updateMatrixWith (NotFlashed n)
    Just (Visited (NotFlashed n)) ->
        if n >= 10
            then flashAndUpdate index (updateMatrixWith Flashed)
            else updateMatrixWith (NotFlashed n)
    _ -> octoMatrix
  where
    updateMatrixWith octo = octoMatrix // [(index, Visited octo)]
    neighbors = genNeighbors index octoMatrix

oneStep :: Matrix (Visited Octopus) -> Matrix (Visited Octopus)
oneStep matrix = newMatrix
  where
    newMatrix = foldr flashAndUpdate matrix indices
    indices = matrix |> bounds |> range

howManyFlashed :: Matrix Octopus -> Int
howManyFlashed = elems .> debug .> count isFlashed

resetFlash :: Octopus -> Octopus
resetFlash =
    \case
        Flashed -> NotFlashed 0
        NotFlashed n -> NotFlashed n

steps :: Int -> Matrix (Visited Octopus) -> Int
steps n matrix
    | n <= 0 = 0
    | otherwise = flashed + steps (n - 1) (amap (fmap resetFlash .> unVisit) newMatrix)
  where
    newMatrix = oneStep matrix
    flashed = howManyFlashed (amap extract newMatrix)
