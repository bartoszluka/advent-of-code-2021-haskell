{-# LANGUAGE LambdaCase #-}

module Day08 (part1) where

import Data.Text (length, split)
import Extra (count)

splitLine :: Text -> Maybe ([Text], [Text])
splitLine input = case split (== '|') input of
    [first, second] -> Just (words first, words second)
    _ -> Nothing

parseInput :: Text -> Maybe [([Text], [Text])]
parseInput = lines .> traverse splitLine

input :: Text
input =
    unlines
        [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
        , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
        , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
        , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
        , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
        , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
        , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
        , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
        , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
        , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
        ]

part1 :: Text -> Maybe Int
part1 input = do
    parsed <- parseInput input
    parsed
        |> map snd
        |> foldMap sumEasyLengths
        |> fold .> getSum
        |> return
  where
    sumEasyLengths = map (Data.Text.length .> easyLenghts .> Sum)
    easyLenghts :: Int -> Int
    easyLenghts = \case
        2 -> 1
        3 -> 1
        4 -> 1
        7 -> 1
        _ -> 0

debug :: Show v => v -> v
debug v = trace (show v) v