{-# LANGUAGE QuasiQuotes #-}

module Day13 where

import qualified Data.Set as Set
import Extra (splitInTwo)
import Text.RawString.QQ (r)

miniInput :: Text
miniInput =
    [r|6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5|]

data FoldAlong
    = X Int
    | Y Int
    deriving (Eq, Show)

type Dot = (Int, Int)

parseInput :: Text -> Maybe (Set Dot, [FoldAlong])
parseInput input = do
    (coordinates, foldInstructions) <- splitInTwo "\n\n" input
    coordinates' <- traverse parseDot (lines coordinates)
    foldInstructions' <- traverse parseFoldInstructions (lines foldInstructions)

    return (fromList coordinates', foldInstructions')

parseFoldInstructions :: Text -> Maybe FoldAlong
parseFoldInstructions input = do
    directionAndIndex <- words input !!? 2
    (direction, index) <- splitInTwo "=" directionAndIndex
    index' <- readMaybe (toString index)
    case direction of
        "x" -> Just <| X index'
        "y" -> Just <| Y index'
        _ -> Nothing

parseDot :: Text -> Maybe Dot
parseDot input = do
    (x, y) <- splitInTwo "," input
    x' <- readMaybe (toString x)
    y' <- readMaybe (toString y)
    return (x', y')
