{-# LANGUAGE NamedFieldPuns #-}

module Day09 (part1, part2) where

import Data.Array.IArray (
    (!),
 )
import Data.List (minimum)
import Extra (choose)
import Matrix (
    Index,
    Matrix,
    allIndices,
    findIndex,
    gen4Neighbors,
    lookup,
    toMatrix,
    updateOne,
 )

data Neighbors a = Neighbors
    { center :: !a
    , neighbors :: ![a]
    }
    deriving (Show)

toSurrounded :: Matrix Int -> [Neighbors Int]
toSurrounded matrix =
    [ Neighbors
        (matrix ! index)
        (findNeighbors (neighbors index))
    | index <- allIndices matrix
    ]
  where
    neighbors idx = gen4Neighbors idx matrix
    findNeighbors = choose (`lookup` matrix)

isMin :: Ord a => Neighbors a -> Bool
isMin Neighbors{center, neighbors} = center < minimum neighbors

part1 :: Text -> Maybe Int
part1 =
    toMatrix id
        .>> toSurrounded
        .>> filter isMin
        .>> foldMap (center .> (+ 1) .> Sum)
        .>> getSum

data Visited = NotVisited | Visited deriving (Show, Eq)

toVisited :: Int -> Visited
toVisited 9 = Visited
toVisited _ = NotVisited

countBasins :: Matrix Visited -> [Int]
countBasins matrix = case findNextNotVisited matrix of
    Nothing -> []
    Just idx ->
        let (oneBasin, newMatrix) = basinSize idx matrix
         in oneBasin : countBasins newMatrix

findNextNotVisited :: Matrix Visited -> Maybe Index
findNextNotVisited = findIndex notVisited
  where
    notVisited Visited = False
    notVisited NotVisited = True

basinSize :: Index -> Matrix Visited -> (Int, Matrix Visited)
basinSize idx matrix = case lookup idx matrix of
    Just NotVisited ->
        let newMatrix = updateOne matrix idx Visited
         in foldr foldMatrix (1, newMatrix) (neighbors idx)
    _ -> (0, matrix)
  where
    neighbors (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
    foldMatrix neighborIndex (n, matrix') =
        let (newSize, matrix'') = basinSize neighborIndex matrix'
         in (n + newSize, matrix'')

part2 :: Text -> Maybe Int
part2 =
    toMatrix id
        .>> fmap toVisited
        .>> countBasins
        .>> sortBy (flip compare)
        .>> take 3
        .>> product
