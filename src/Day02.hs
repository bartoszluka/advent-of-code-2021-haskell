{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day02 (part1, part2, parseCommand) where

import Extra (choose)
import Prelude hiding (Down)

data Command
    = Forward Int
    | Down Int
    | Up Int
    deriving (Eq, Show)

parseCommand :: Text -> Maybe Command
parseCommand line = do
    (direction, number) <- splitWords line
    case (direction, number) of
        ("forward", n) -> toCommand Forward n
        ("down", n) -> toCommand Down n
        ("up", n) -> toCommand Up n
        _ -> Nothing
  where
    splitWords input = case words input of
        [direction, number] -> Just (direction, number)
        _ -> Nothing

    toCommand :: (Int -> Command) -> Text -> Maybe Command
    toCommand command = toString .> readMaybe .> fmap command

foldCommands :: [Command] -> (Int, Int)
foldCommands =
    foldl'
        ( \(horiz, depth) -> \case
            Forward value -> (horiz + value, depth)
            Up value -> (horiz, depth - value)
            Down value -> (horiz, depth + value)
        )
        (0, 0)

depthTimesTravelled :: [Text] -> Int
depthTimesTravelled lines =
    horizontal * depth
  where
    (horizontal, depth) = foldCommands parsed
    parsed = choose parseCommand lines

foldCommands2 :: [Command] -> (Int, Int, Int)
foldCommands2 =
    foldl'
        ( \(horiz, depth, aim) -> \case
            Down value -> (horiz, depth, aim + value)
            Up value -> (horiz, depth, aim - value)
            Forward value -> (horiz + value, depth + aim * value, aim)
        )
        (0, 0, 0)

depthTimesTravelled2 :: [Text] -> Int
depthTimesTravelled2 lines =
    horizontal * depth
  where
    parsed = choose parseCommand lines
    (horizontal, depth, _) = foldCommands2 parsed

part1 :: [Text] -> Int
part1 = depthTimesTravelled

part2 :: [Text] -> Int
part2 = depthTimesTravelled2