module Day11 where

import Extra (count)
import Matrix (
    Index,
    Matrix,
    allIndices,
    elements,
    gen8Neighbors,
    lookup,
    toMatrix,
    updateOne,
 )
import qualified Text.Show

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

flashAndUpdate :: Index -> Matrix Octopus -> Matrix Octopus
flashAndUpdate index octoMatrix = case lookup index octoMatrix of
    Just (NotFlashed n) ->
        if n >= 9
            then foldr flashAndUpdate (updateMatrixWith Flashed) neighbors
            else updateMatrixWith (NotFlashed (n + 1))
    _ -> octoMatrix
  where
    updateMatrixWith = updateOne octoMatrix index
    neighbors = gen8Neighbors index octoMatrix

oneStep :: Matrix Octopus -> Matrix Octopus
oneStep matrix = newMatrix
  where
    newMatrix = foldr flashAndUpdate matrix (allIndices matrix)

howManyFlashed :: Matrix Octopus -> Int
howManyFlashed = elements .> count isFlashed

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
part1 = toMatrix NotFlashed .>> steps 100