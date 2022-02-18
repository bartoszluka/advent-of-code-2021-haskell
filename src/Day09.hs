module Day09 where

import Data.Array.IArray (IArray)
import Data.Array.Unboxed (
    Array,
    assocs,
    bounds,
    inRange,
    listArray,
    (!),
 )
import Data.Ix (Ix)

miniInput :: Text
miniInput =
    unlines
        [ "2199943210"
        , "3987894921"
        , "9856789892"
        , "8767896789"
        , "9899965678"
        ]

type Matrix = Array Int (Array Int Int)

toMatrix :: Text -> Maybe Matrix
toMatrix = lines .> traverse oneLine .> fmap toArray
  where
    oneLine :: Text -> Maybe (Array Int Int)
    oneLine = toString .> traverse (one .> readMaybe) .> fmap toArray

    toArray list = listArray (1, length list) list

safeIndex :: (Ix i, IArray ix el) => i -> ix i el -> Maybe el
safeIndex idx arr =
    if inRange (bounds arr) idx
        then Just <| arr ! idx
        else Nothing

dualIndex :: (Int, Int) -> Matrix -> Maybe Int
dualIndex (i, j) = safeIndex i >=> safeIndex j

data Surrounded a = Surrounded
    { surrounded :: a
    , surrounding :: [a]
    }

toSurrounded :: Matrix -> [Surrounded Int]
toSurrounded matrix = matrix |> assocs |> undefined