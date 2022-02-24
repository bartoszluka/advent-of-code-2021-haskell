{-# LANGUAGE FlexibleInstances #-}

module Day13 (part1, part2, part2Show) where

import Data.Array.IArray (Ix (range))
import Data.List (groupBy)
import qualified Data.Set as Set
import qualified Data.Text as T
import Extra (mapFst, mapSnd, readMaybeText, splitInTwo)
import qualified Text.Show

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

foldAlong :: Set Dot -> FoldAlong -> Set Dot
foldAlong set = \case
    X n -> foldSet fst mapFst n set
    Y n -> foldSet snd mapSnd n set
  where
    foldSet selector selectorMap n =
        Set.filter (selector .> (/= n))
            .> Set.partition (selector .> (< n))
            .> (\(lessThan, greaterThan) -> lessThan <> invertCoordinates greaterThan)
      where
        -- y -> (y - 2 * (y - n))
        -- y -> (y - 2y + 2n)
        -- y -> (2n - y)
        invertCoordinates = Set.map (selectorMap (\coordinate -> 2 * n - coordinate))

part1 :: Text -> Maybe Int
part1 = parseInput .> fmap (mapSnd (take 1)) .> fmap (uncurry (foldl' foldAlong) .> Set.size)

newtype PrintableSet = PrintableSet (Set Dot)

instance Show PrintableSet where
    show (PrintableSet set) =
        set
            |> printTransparentPaper
            |> ("\n" <>)
            |> toString

printTransparentPaper :: Set Dot -> Text
printTransparentPaper set =
    range ((0, 0), (findMax fst, findMax snd))
        |> groupBy (on (==) fst)
        |> transpose
        |> map
            ( map
                ( \point ->
                    if point `Set.member` set
                        then '#'
                        else ' '
                )
                .> toText
                .> T.intersperse ' '
            )
        |> unlines
  where
    findMax selector = set |> Set.map selector .> Set.findMax

part2 :: Text -> Maybe (Set Dot)
part2 = parseInput .> fmap (uncurry (foldl' foldAlong))

part2Show :: Text -> Maybe PrintableSet
part2Show = part2 .>> PrintableSet
