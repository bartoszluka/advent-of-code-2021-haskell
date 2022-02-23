module Day11 where

import Data.Array.IArray (
    IArray,
    Ix,
    array,
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
import Extra (count)
import qualified Text.Show

type Index = (Int, Int)

type Matrix a = Array Index a

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

flashAndUpdate :: Index -> Matrix Octopus -> Matrix Octopus
flashAndUpdate index octoMatrix = case safeIndex index octoMatrix of
    Just (NotFlashed n) ->
        if n >= 9
            then foldr flashAndUpdate (updateMatrixWith Flashed) neighbors
            else updateMatrixWith (NotFlashed (n + 1))
    _ -> octoMatrix
  where
    updateMatrixWith octopus = octoMatrix // one (index, octopus)
    neighbors = genNeighbors index octoMatrix

oneStep :: Matrix Octopus -> Matrix Octopus
oneStep matrix = newMatrix
  where
    newMatrix = foldr flashAndUpdate matrix indices
    indices = matrix |> bounds |> range

howManyFlashed :: Matrix Octopus -> Int
howManyFlashed = elems .> count isFlashed

resetFlash :: Octopus -> Octopus
resetFlash =
    \case
        Flashed -> NotFlashed 0
        NotFlashed n -> NotFlashed n

steps :: Int -> Matrix Octopus -> Int
steps n matrix
    | n <= 0 = howManyFlashed matrix
    | otherwise =
        let newMatrix = oneStep matrix
         in howManyFlashed newMatrix + steps (n - 1) (resetFlash <$> newMatrix)

part1 :: Text -> Maybe Int
part1 = toMatrix .>> steps 100