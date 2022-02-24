{-# LANGUAGE QuasiQuotes #-}

module Day13 where

import Control.Monad (liftM2)
import Data.Array.IArray (Ix (range))
import Data.List (groupBy)
import Data.Sequence (chunksOf)
import qualified Data.Set as Set
import qualified Data.Text as T
import Extra (mapFst, mapSnd, printInGhci, readMaybeText, splitInTwo)
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
    index' <- readMaybeText index
    case direction of
        "x" -> Just <| X index'
        "y" -> Just <| Y index'
        _ -> Nothing

parseDot :: Text -> Maybe Dot
parseDot input = do
    (x, y) <- splitInTwo "," input
    x' <- readMaybeText x
    y' <- readMaybeText y
    return (x', y')

data Paper = Paper
    { dotSet :: !(Set Dot)
    , xSize :: !Int
    , ySize :: Int
    }

foldAlong :: Set Dot -> FoldAlong -> Set Dot
foldAlong set = \case
    X n -> foldSet fst mapFst n set
    Y n -> foldSet snd mapSnd n set
  where
    foldSet whichElement mapping n =
        Set.filter (whichElement .> (/= n))
            .> Set.partition (whichElement .> (< n))
            .> (\(lessThan, greaterThan) -> lessThan <> invertCoordinates greaterThan)
      where
        invertCoordinates = Set.map (mapping (\xy -> 2 * n - xy))

part1 :: Text -> Maybe (Set Dot)
part1 = parseInput .> fmap (mapSnd (take 1)) .> fmap (uncurry (foldl' foldAlong))

debug v = trace (show v) v

printTransparentPaper :: Set Dot -> [Text]
printTransparentPaper set =
    case Set.lookupMax set of
        Nothing -> []
        Just bound ->
            range ((0, 0), bound)
                |> groupBy (on (==) fst)
                |> map
                    ( map
                        ( \point ->
                            if point `Set.member` set
                                then '#'
                                else '.'
                        )
                        .> toText
                    )

part2 :: Text -> Maybe (Set Dot)
part2 = parseInput .> fmap (uncurry (foldl' foldAlong))
