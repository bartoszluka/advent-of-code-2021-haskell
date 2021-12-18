{-# LANGUAGE LambdaCase #-}

module Day02 (part1, part2) where

import Data.List (partition)
import Extra (choose, readMaybeInt)

data Command
    = Forward Int
    | Down Int
    | Up Int

parseCommand :: String -> Maybe Command
parseCommand line =
    case words line of
        ["forward", val] -> case readMaybeInt val of
            Just int -> Just $ Forward int
            Nothing -> Nothing
        ["down", val] -> case readMaybeInt val of
            Just int -> Just $ Down int
            Nothing -> Nothing
        ["up", val] -> case readMaybeInt val of
            Just int -> Just $ Up int
            Nothing -> Nothing
        _ -> Nothing

splitCommands :: [Command] -> ([Command], [Command])
splitCommands =
    partition
        ( \case
            Forward _ -> True
            _ -> False
        )

sumCommands :: [Command] -> Int
sumCommands =
    sum
        . map
            ( \case
                Forward int -> int
                Down int -> int
                Up int -> negate int
            )

depthTimesTravelled :: [String] -> Int
depthTimesTravelled lines =
    horizontal * depth
  where
    parsed = choose parseCommand lines
    (horizontal, depth) = foldCommands parsed

foldCommands :: [Command] -> (Int, Int)
foldCommands =
    foldl
        ( \(horiz, depth) -> \case
            Forward value -> (horiz + value, depth)
            Up value -> (horiz, depth - value)
            Down value -> (horiz, depth + value)
        )
        (0, 0)

foldCommands2 :: [Command] -> (Int, Int, Int)
foldCommands2 =
    foldl
        ( \(horiz, depth, aim) -> \case
            Down value -> (horiz, depth, aim + value)
            Up value -> (horiz, depth, aim - value)
            Forward value -> (horiz + value, depth + aim * value, aim)
        )
        (0, 0, 0)

depthTimesTravelled2 :: [String] -> Int
depthTimesTravelled2 lines =
    horizontal * depth
  where
    parsed = choose parseCommand lines
    (horizontal, depth, _) = foldCommands2 parsed

part1 :: [String] -> Int
part1 = depthTimesTravelled

part2 :: [String] -> Int
part2 = depthTimesTravelled2