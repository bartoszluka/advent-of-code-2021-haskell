{-# LANGUAGE DeriveGeneric #-}

module Day12 where

import Data.Char (isUpper)
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.Text as T

data Cave
    = StartCave
    | EndCave
    | BigCave Text
    | SmallCave Text
    deriving (Show, Eq, Generic)

instance Hashable Cave

toCave :: Text -> Cave
toCave "start" = StartCave
toCave "end" = EndCave
toCave label
    | T.any isUpper label = BigCave label
    | otherwise = SmallCave label

miniInput :: Text
miniInput =
    unlines
        [ "start-A"
        , "start-b"
        , "A-c"
        , "A-b"
        , "b-d"
        , "A-end"
        , "b-end"
        ]

parseInput :: Text -> Maybe [(Cave, Cave)]
parseInput = lines .> traverse parseLine

parseLine :: Text -> Maybe (Cave, Cave)
parseLine input = do
    (label1, label2) <- splitInTwo "-" input
    return (toCave label1, toCave label2)

splitInTwo :: Text -> Text -> Maybe (Text, Text)
splitInTwo sep text = case T.splitOn sep text of
    [x, y] -> Just (x, y)
    _ -> Nothing

type CaveSystem = HashMap Cave (HashSet Cave)

toCaveSystem :: [(Cave, Cave)] -> CaveSystem
toCaveSystem caves =
    caves ++ map swap caves
        |> map (mapSnd one)
        |> HMap.fromListWith HSet.union
  where
    mapSnd f (a, b) = (a, f b)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = x >>= f
