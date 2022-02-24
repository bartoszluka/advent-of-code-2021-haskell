{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}

module Day12 (part1, part2) where

import Data.Char (isUpper)
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.Text as T
import Extra (countEachHash, splitInTwo)
import qualified Text.Show

data Cave
    = StartCave
    | EndCave
    | BigCave Text
    | SmallCave Text
    deriving (Eq, Generic)

instance Hashable Cave

instance Show Cave where
    show StartCave = "start"
    show EndCave = "end"
    show (BigCave label) = toString label
    show (SmallCave label) = toString label

toCave :: Text -> Cave
toCave "start" = StartCave
toCave "end" = EndCave
toCave label
    | T.any isUpper label = BigCave label
    | otherwise = SmallCave label

parseInput :: Text -> Maybe [(Cave, Cave)]
parseInput = lines .> traverse parseLine

parseLine :: Text -> Maybe (Cave, Cave)
parseLine input = do
    (label1, label2) <- splitInTwo "-" input
    return (toCave label1, toCave label2)

type CaveSystem = HashMap Cave (HashSet Cave)

toCaveSystem :: [(Cave, Cave)] -> CaveSystem
toCaveSystem caves =
    caves ++ map swap caves
        |> map (mapSnd one)
        |> HMap.fromListWith HSet.union
  where
    mapSnd f (a, b) = (a, f b)

part1 :: Text -> Maybe Int
part1 =
    parseInput
        .>> toCaveSystem
        .>> findAllPaths (const False)
        .>> length

findAllPaths :: ([Cave] -> Bool) -> CaveSystem -> [[Cave]]
findAllPaths duplicates system =
    case HMap.lookup StartCave system of
        Nothing -> []
        Just startingCaves -> concatMap (findPath [] [StartCave]) startingCaves |> map reverse
  where
    findPath :: [[Cave]] -> [Cave] -> Cave -> [[Cave]]
    findPath foundPaths path currentCave =
        case currentCave of
            StartCave -> foundPaths
            EndCave -> (currentCave : path) : foundPaths
            bigCave@(BigCave _) ->
                let newPath = bigCave : path
                 in concatMap (findPath foundPaths newPath) (nextCaves bigCave)
            smallCave@(SmallCave _) ->
                if smallCave `notElem` path || duplicates path
                    then concatMap (findPath foundPaths (smallCave : path)) (nextCaves smallCave)
                    else foundPaths

    nextCaves cave = HMap.lookup cave system |> fromMaybe []

noDuplicateSmallCaves :: [Cave] -> Bool
noDuplicateSmallCaves path =
    path |> filter isSmallCave |> countEachHash |> map snd |> all (<= 1)
  where
    isSmallCave (SmallCave _) = True
    isSmallCave _ = False

part2 :: Text -> Maybe Int
part2 =
    parseInput
        .>> toCaveSystem
        .>> findAllPaths noDuplicateSmallCaves
        .>> length
