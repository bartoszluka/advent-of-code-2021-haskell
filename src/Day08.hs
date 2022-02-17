{-# LANGUAGE OverloadedLists #-}

module Day08 (part1, part2) where

import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Relude.Extra (bimapBoth)

splitLine :: Text -> Maybe ([Text], [Text])
splitLine input = case Text.split (== '|') input of
    [firstPart, secondPart] -> Just <| bimapBoth words (firstPart, secondPart)
    _ -> Nothing

parseInput :: Text -> Maybe [([Text], [Text])]
parseInput = lines .> traverse splitLine

part1 :: Text -> Maybe Int
part1 input = do
    parsed <- parseInput input
    parsed
        |> map snd
        |> foldMap sumEasyLengths
        |> fold .> getSum
        |> return
  where
    sumEasyLengths = map (Text.length .> easyLenghts .> Sum)
    easyLenghts :: Int -> Int
    easyLenghts = \case
        2 -> 1
        3 -> 1
        4 -> 1
        7 -> 1
        _ -> 0

type Segment = Char

toSet :: Text -> Set Segment
toSet = toString .> Set.fromList

fromSingleton :: Set a -> Maybe a
fromSingleton set = case Set.toAscList set of
    [item] -> Just item
    _ -> Nothing

findInSet :: (a -> Bool) -> Set a -> Maybe a
findInSet finder set = Set.filter finder set |> fromSingleton

solveDigits :: Set (Set Segment) -> Maybe (Map (Set Segment) Char)
solveDigits set = do
    -- easy digits
    digit1 <- findBySize 2 set
    digit4 <- findBySize 4 set
    digit7 <- findBySize 3 set
    digit8 <- findBySize 7 set

    -- unused
    -- a <- fromSingleton <| digit7 \\ digit1
    digit3 <- findInSet (\s -> (Set.size s == 5) && digit1 `Set.isSubsetOf` s) set
    digit6 <- findInSet (\s -> (Set.size s == 6) && (not <| digit1 `Set.isSubsetOf` s)) set
    digit9 <- findInSet (\s -> (Set.size s == 6) && (digit4 `Set.isSubsetOf` s)) set

    b <- fromSingleton <| digit9 \\ digit3
    e <- fromSingleton <| digit8 \\ digit9
    f <- fromSingleton <| digit6 `Set.intersection` digit1
    c <- fromSingleton <| digit9 \\ digit6
    -- unused
    -- g <- fromSingleton <| digit9 \\ digit4 \\ [a]
    d <- fromSingleton <| digit4 \\ digit1 \\ [b]

    return
        [ (digit1, '1')
        , (digit8 \\ [b, f], '2')
        , (digit3, '3')
        , (digit4, '4')
        , (digit8 \\ [c, e], '5')
        , (digit6, '6')
        , (digit7, '7')
        , (digit8, '8')
        , (digit9, '9')
        , (digit8 \\ [d], '0')
        ]
  where
    findBySize n = findInSet (Set.size .> (== n))

decodeOutput :: Map (Set Segment) Char -> [Set Segment] -> Maybe Int
decodeOutput mappings = traverse (`Map.lookup` mappings) >=> readMaybe

part2 :: Text -> Maybe Int
part2 input = do
    parsed <- parseInput input
    parsed
        |> traverse singleLine
        |> fmap sum
  where
    singleLine :: ([Text], [Text]) -> Maybe Int
    singleLine (inputs, outputs) =
        solveDigits inputSets >>= flip decodeOutput outputSets
      where
        inputSets = inputs |> map toSet |> Set.fromList
        outputSets = outputs |> map toSet