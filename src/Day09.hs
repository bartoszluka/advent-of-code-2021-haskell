{-# LANGUAGE NamedFieldPuns #-}

module Day09 (part1, part2) where

import Data.Array.IArray (IArray)
import Data.Array.Unboxed (
    Array,
    amap,
    array,
    assocs,
    bounds,
    inRange,
    range,
    (!),
    (//),
 )
import Data.Ix (Ix)
import Data.List (maximum, minimum)
import Extra (choose)

type Index = (Int, Int)
type Matrix a = Array Index a

autoArray :: [(Index, Int)] -> Matrix Int
autoArray list = array ((1, 1), (i, j)) list
  where
    i = list |> map (fst .> fst) |> maximum
    j = list |> map (fst .> snd) |> maximum

toMatrix :: Text -> Maybe (Matrix Int)
toMatrix =
    lines .> traverse oneLine
        .> fmap
            ( indexed
                .> concatMap addIndexes
                .> autoArray
            )
  where
    oneLine :: Text -> Maybe [Index]
    oneLine = toString .> traverse (one .> readMaybe) .> fmap indexed
    addIndexes (idx, list) = map (\(innerIdx, val) -> ((idx, innerIdx), val)) list
    indexed = zip [1 ..]

safeIndex :: (Ix i, IArray ix el) => i -> ix i el -> Maybe el
safeIndex idx arr =
    if inRange (bounds arr) idx
        then Just <| arr ! idx
        else Nothing

data Surrounded a = Surrounded
    { surrounded :: !a
    , surrounding :: ![a]
    }
    deriving (Show)

toSurrounded :: Matrix Int -> [Surrounded Int]
toSurrounded matrix =
    [ Surrounded
        (matrix ! idx)
        (findNeighbors neighbors matrix)
    | (idx, neighbors) <- genNeighbors matrix
    ]

findNeighbors :: (Ix i, IArray ix b) => [i] -> ix i b -> [b]
findNeighbors unsafeIndices matrix = map (`safeIndex` matrix) unsafeIndices |> choose id

genNeighbors :: Matrix Int -> [(Index, [Index])]
genNeighbors matrix =
    [ ( (i, j)
      , [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
      )
    | (i, j) <- range <. bounds <| matrix
    ]

isMin :: Ord a => Surrounded a -> Bool
isMin Surrounded{surrounded, surrounding} = surrounded < minimum surrounding

part1 :: Text -> Maybe Int
part1 =
    toMatrix
        .>> toSurrounded
        .>> filter isMin
        .>> foldMap (surrounded .> (+ 1) .> Sum)
        .>> getSum

data Visited = NotVisited | Visited deriving (Show, Eq)

toVisited :: Matrix Int -> Matrix Visited
toVisited =
    amap
        ( \case
            9 -> Visited
            _ -> NotVisited
        )

countBasins :: Matrix Visited -> [Int]
countBasins matrix = case findNextNotVisited matrix of
    Nothing -> []
    Just idx ->
        let (oneBasin, newMatrix) = basinSize idx matrix
         in oneBasin : countBasins newMatrix

findNextNotVisited :: Matrix Visited -> Maybe Index
findNextNotVisited = assocs .> find notVisited .>> fst
  where
    notVisited (_, Visited) = False
    notVisited (_, NotVisited) = True

basinSize :: Index -> Matrix Visited -> (Int, Matrix Visited)
basinSize idx matrix = case safeIndex idx matrix of
    Just NotVisited ->
        let newMatrix = matrix // [(idx, Visited)]
         in foldr foldMatrix (1, newMatrix) (neighbors idx)
    _ -> (0, matrix)
  where
    neighbors (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
    foldMatrix neighborIndex (n, matrix') =
        let (newSize, matrix'') = basinSize neighborIndex matrix'
         in (n + newSize, matrix'')

part2 :: Text -> Maybe Int
part2 =
    toMatrix
        .>> toVisited
        .>> countBasins
        .>> sortBy (flip compare)
        .>> take 3
        .>> product
