{-# LANGUAGE QuasiQuotes #-}

module Day14 where

import Control.Monad (liftM2)
import Data.Array.Unboxed (Array, accumArray, assocs)
import Data.Foldable (maximum, minimum)
import qualified Data.Map.Strict as Map
import Extra (pairs, splitInTwo)
import Text.RawString.QQ (r)
import qualified Text.Show

miniInput :: Text
miniInput =
    [r|NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C|]

newtype Polymer = Polymer (Char, Char)
    deriving (Eq, Ord)

instance Show Polymer where
    show (Polymer (x, y)) = [x, y]

newtype PolymerRules = PolymerRules (Map Polymer Char)

-- for pretty printing
instance Show PolymerRules where
    show (PolymerRules rules) =
        rules
            |> Map.toList
            |> map (\(polymer, char) -> show polymer <> " -> " <> one char)
            |> unlines
            |> ("\n" <>)
            |> toString

parseInput :: Text -> Maybe ([Char], Map Polymer Char)
parseInput input = do
    (template, rules) <- splitInTwo "\n\n" input
    polymerRules <- traverse parseRule <| lines rules

    return (toString template, Map.fromList polymerRules)

parseRule :: Text -> Maybe (Polymer, Char)
parseRule rule = do
    (left, right) <- splitInTwo " -> " rule
    polymer <- asPolymer (toString left)
    right' <- asOne (toString right)

    return (polymer, right')
  where
    asOne = \case
        [single] -> Just single
        _ -> Nothing

    asPolymer = \case
        [x, y] -> Just <| Polymer (x, y)
        _ -> Nothing

polymerize :: [Char] -> Map Polymer Char -> [Char]
polymerize template rules =
    template
        |> pairs
        |> map Polymer
        |> map expandRule
        |> concatWithoutDuplicates
  where
    expandRule :: Polymer -> PairOrTriple Char
    expandRule polymer@(Polymer (x, y)) = case polymer `Map.lookup` rules of
        Nothing -> Pair (x, y)
        Just output -> Triple (x, output, y)

data PairOrTriple a = Pair (a, a) | Triple (a, a, a)

concatWithoutDuplicates :: [PairOrTriple Char] -> [Char]
concatWithoutDuplicates = \case
    [] -> []
    [Pair (a, b)] -> [a, b]
    [Triple (a, b, c)] -> [a, b, c]
    Triple (a, b, _) : rest -> [a, b] ++ (concatWithoutDuplicates <! rest)
    Pair (a, _) : rest -> a : (concatWithoutDuplicates <! rest)

-- use array for linear counting
countChars :: [Char] -> [(Char, Integer)]
countChars = flip zip (repeat 1) .> toArray .> assocs .> filter (snd .> (> 0))
  where
    toArray :: [(Char, Integer)] -> Array Char Integer
    toArray = accumArray (+) 0 (('A', 'Z') :: (Char, Char))

steps :: Integer -> [Char] -> Map Polymer Char -> [Char]
steps n template rules =
    foldl'
        (\newTemplate _ -> polymerize newTemplate rules)
        template
        [1 .. n]

solution :: Integer -> Text -> Maybe Integer
solution n =
    parseInput .>> uncurry (steps n) .>> countChars
        .>> map snd
        .>> calc maximum (-) minimum
  where
    calc = flip liftM2

part1 :: Text -> Maybe Integer
part1 = solution 10

part2 :: Text -> Maybe Integer
part2 = solution 40
