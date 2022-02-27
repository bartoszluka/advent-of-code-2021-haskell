{-# LANGUAGE QuasiQuotes #-}

module Day14 where

import Control.Monad (liftM2)
import Data.Array.Unboxed (Array, accum, accumArray, array, assocs, (//))
import Data.Foldable (maximum, minimum)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
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
    Triple (a, b, _) : rest -> [a, b] ++ concatWithoutDuplicates rest
    Pair (a, _) : rest -> a : concatWithoutDuplicates rest

-- use array for linear counting
countChars :: [Char] -> [(Char, Integer)]
countChars =
    zipWithOnes
        .> toArray
        .> assocs
        .> filter (snd .> (> 0))
  where
    toArray :: [(Char, Integer)] -> Array Char Integer
    toArray = accumArray (+) 0 (('A', 'Z') :: (Char, Char))
    zipWithOnes = flip zip (repeat 1)

steps :: Integer -> [Char] -> Map Polymer Char -> [Char]
steps n template rules =
    foldl'
        (\newTemplate _ -> polymerize newTemplate rules)
        template
        [1 .. n]

solution :: Integer -> Text -> Maybe Integer
solution n =
    parseInput
        .>> applyToPair (steps n)
        .>> countChars
        .>> map snd
        .>> calc maximum (-) minimum
  where
    calc = flip liftM2
    applyToPair = uncurry

part1 :: Text -> Maybe Integer
part1 = solution 10

part2 :: Text -> Maybe Integer
part2 = solution 40

main :: IO ()
main = print <| fromMaybe 0 <| solution 18 miniInput

type Counts = Array Char Integer
charArray :: [(Char, Integer)] -> Counts
charArray = array ('A', 'Z')

zeros :: Counts
zeros = charArray (zip ['A' .. 'Z'] (repeat 0))

zerosWith :: [Char] -> Counts
zerosWith chars = zeros // zip chars (repeat 1)

polymerizeWithArray :: Int -> Map Polymer Char -> Polymer -> Counts
polymerizeWithArray 0 _ (Polymer (x, _)) = zerosWith [x]
polymerizeWithArray depth rules polymer@(Polymer (x, y)) = case Map.lookup polymer rules of
    Nothing -> zerosWith [x]
    Just c ->
        let poly char1 char2 = polymerizeWithArray (depth - 1) rules (Polymer (char1, char2))
         in mergeArrays
                (poly x c)
                (poly c y)

mergeArrays :: Counts -> Counts -> Counts
mergeArrays chars chars' = accum (+) chars (assocs chars')

-- NN
-- NCN
-- NBCCN
-- NBBBCNCCN
-- NBBNBNBBCCNBCNCCN

rul :: Map Polymer Char
rul = parseInput miniInput |> fmap snd |> fromMaybe Map.empty

countPolymers :: Int -> Map Polymer Char -> [Char] -> [(Char, Integer)]
countPolymers depth rules template =
    let polymers = toPolymers template
     in map (polymerizeWithArray depth rules) polymers
            |> foldl' mergeArrays zeros
            |> assocs
            |> filter (snd .> (> 0))
  where
    toPolymers = pairs .> map Polymer
