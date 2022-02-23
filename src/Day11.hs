module Day11 (part1, part2) where

import Extra (count)
import Matrix (
    Index,
    Matrix,
    allIndices,
    elements,
    gen8Neighbors,
    lookup,
    size,
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

whenAllFlashed :: Matrix Octopus -> Int
whenAllFlashed = whenAllFlashed' 0
  where
    whenAllFlashed' currentStep matrix
        | allFlashed = currentStep
        | otherwise =
            matrix
                |> (resetFlash <$>)
                |> oneStep
                |> whenAllFlashed' (currentStep + 1)
      where
        allFlashed = howManyFlashed matrix == size matrix

part2 :: Text -> Maybe Int
part2 = toMatrix NotFlashed .>> whenAllFlashed
