module Matrix where

import Data.Array.IArray (
    IArray,
    Ix,
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

type Index = (Int, Int)

type Matrix a = Array Index a

autoArray :: [(Index, a)] -> Matrix a
autoArray list = array ((1, 1), (i, j)) list
  where
    i = list |> map (fst .> fst) |> maximum
    j = list |> map (fst .> snd) |> maximum

toMatrix :: (Int -> a) -> Text -> Maybe (Matrix a)
toMatrix mapping =
    lines .> traverse oneLine
        .>> indexed
        .>> concatMap addIndices
        .>> map (mapSnd mapping)
        .>> autoArray
  where
    oneLine :: Text -> Maybe [Index]
    oneLine = toString .> traverse (one .> readMaybe) .> fmap indexed
    addIndices (index, list) = map (\(innerIdx, val) -> ((index, innerIdx), val)) list
    indexed = zip [1 ..]
    mapSnd function (x, y) = (x, function y)

lookup :: (Ix i, IArray ix el) => i -> ix i el -> Maybe el
lookup idx arr =
    if inRange (bounds arr) idx
        then Just <| arr ! idx
        else Nothing

gen8Neighbors :: Index -> Matrix a -> [Index]
gen8Neighbors index@(i, j) matrix =
    case lookup index matrix of
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

gen4Neighbors :: Index -> Matrix a -> [Index]
gen4Neighbors index@(i, j) matrix =
    case lookup index matrix of
        Nothing -> []
        Just _ ->
            [ (i, j - 1)
            , (i, j + 1)
            , (i - 1, j)
            , (i + 1, j)
            ]

allIndices :: Matrix a -> [Index]
allIndices = bounds .> range

updateOne ::
    (IArray array elements, Ix index) =>
    array index elements ->
    index ->
    elements ->
    array index elements
updateOne matrix index item = matrix // one (index, item)

elements :: Matrix a -> [a]
elements = elems

findIndex :: (el -> Bool) -> Matrix el -> Maybe Index
findIndex predicate = assocs .> find (snd .> predicate) .>> fst