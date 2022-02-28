module Day14 (part1, part2) where

import Data.Array.Unboxed (Array, accumArray, assocs)
import Data.Foldable (maximum, minimum)
import qualified Data.Map.Strict as Map
import Extra (calc, pairs, splitInTwo, timesApply)
import qualified Text.Show

newtype Polymer = Polymer (Char, Char)
    deriving (Eq, Ord)

instance Show Polymer where
    show (Polymer (x, y)) = [x, y]

parseInput :: Text -> Maybe (Map Polymer Char, NonEmpty Char)
parseInput input = do
    (template, rules) <- splitInTwo "\n\n" input
    polymerRules <- traverse parseRule <| lines rules
    template' <- nonEmpty <| toString template
    return (Map.fromList polymerRules, template')

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

polymerize :: Map Polymer Char -> [Char] -> [Char]
polymerize rules template =
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
    toArray = accumArray (+) 0 ('A', 'Z')
    zipWithOnes = flip zip (repeat 1)

naiveSolution :: Int -> Text -> Maybe Integer
naiveSolution n =
    parseInput
        .>> uncurry steps
        .>> countChars
        .>> map snd
        .>> calc maximum (-) minimum
  where
    steps rules = toList .> (n `timesApply` polymerize rules)

part1 :: Text -> Maybe Integer
part1 = naiveSolution 10

-- part 2 is done differently because the first solution grows exponentially

polymerStep :: Map Polymer Char -> Map Polymer Integer -> Map Polymer Integer
polymerStep rules =
    Map.assocs .> map expandRule .> Map.unionsWith (+)
  where
    expandRule (polymer@(Polymer (a, b)), n) = case Map.lookup polymer rules of
        Nothing -> one (polymer, n)
        Just expanded ->
            Map.fromListWith
                (+)
                [ (Polymer (a, expanded), n)
                , (Polymer (expanded, b), n)
                ]

polymerizeNTimes :: Int -> Map Polymer Char -> NonEmpty Char -> Integer
polymerizeNTimes n rules template =
    let initialCounts = template |> toPolymers |> withOnes |> Map.fromListWith (+)
     in initialCounts
            |> n `timesApply` polymerStep rules
            |> Map.mapKeysWith (+) (\(Polymer (_, b)) -> b)
            |> Map.adjust (+ 1) (head template)
            |> calc maximum (-) minimum
  where
    toPolymers :: NonEmpty Char -> [Polymer]
    toPolymers = toList .> pairs .> map Polymer
    withOnes = flip zip (repeat 1)

solution :: Int -> Text -> Maybe Integer
solution n =
    parseInput .>> uncurry (polymerizeNTimes n)

part2 :: Text -> Maybe Integer
part2 = solution 40
