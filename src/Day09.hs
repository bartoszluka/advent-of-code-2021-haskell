{-# LANGUAGE NamedFieldPuns #-}

module Day09 where

import Data.Array.IArray (IArray)
import Data.Array.Unboxed (
    Array,
    array,
    bounds,
    inRange,
    range,
    (!),
 )
import Data.Ix (Ix)
import Data.List (maximum, minimum)
import Extra (choose)

miniInput :: Text
miniInput =
    unlines
        [ "2199943210"
        , "3987894921"
        , "9856789892"
        , "8767896789"
        , "9899965678"
        ]

type Matrix = Array (Int, Int) Int

autoArray :: [((Int, Int), Int)] -> Matrix
autoArray list = array ((1, 1), (i, j)) list
  where
    i = list |> map (fst .> fst) |> maximum
    j = list |> map (fst .> snd) |> maximum

toMatrix :: Text -> Maybe Matrix
toMatrix =
    lines .> traverse oneLine
        .> fmap
            ( indexed
                .> concatMap addIndexes
                .> autoArray
            )
  where
    oneLine :: Text -> Maybe [(Int, Int)]
    oneLine = toString .> traverse (one .> readMaybe) .> fmap indexed
    addIndexes (idx, list) = map (\(innerIdx, val) -> ((idx, innerIdx), val)) list
    indexed = zip [1 ..]

safeIndex :: (Ix i, IArray ix el) => i -> ix i el -> Maybe el
safeIndex idx arr =
    if inRange (bounds arr) idx
        then Just <| arr ! idx
        else Nothing

data Surrounded a = Surrounded
    { surrounded :: a
    , surrounding :: [a]
    }
    deriving (Show)

toSurrounded :: Matrix -> [Surrounded Int]
toSurrounded matrix =
    [ Surrounded
        (matrix ! idx)
        (findNeighbors neighbors matrix)
    | (idx, neighbors) <- genNeighbors (bounds matrix)
    ]

findNeighbors :: (Ix i, IArray ix b) => [i] -> ix i b -> [b]
findNeighbors unsafeIndices matrix = map (`safeIndex` matrix) unsafeIndices |> choose id

genNeighbors :: (Ix i, Ix j, Num i, Num j) => ((i, j), (i, j)) -> [((i, j), [(i, j)])]
genNeighbors bnds = [((i, j), [(i -1, j), (i + 1, j), (i, j -1), (i, j + 1)]) | (i, j) <- range bnds]

isMin :: Ord a => Surrounded a -> Bool
isMin Surrounded{surrounded, surrounding} = surrounded < minimum surrounding

part1 :: Text -> Maybe Int
part1 =
    toMatrix
        .> fmap
            ( toSurrounded
                .> filter isMin
                .> foldMap (surrounded .> (+ 1) .> Sum)
                .> getSum
            )
