{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}

module Day12 where

import Data.Char (isUpper)
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.Text as T
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

printPath :: [Cave] -> Text
printPath = map show .> T.intercalate " -> "

printIO :: Maybe [Text] -> IO ()
printIO = fmap (unlines .> toString) .> fromMaybe "" .> putStrLn

findAllPaths :: CaveSystem -> [[Cave]]
findAllPaths system =
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
                if smallCave `elem` path
                    then foundPaths
                    else concatMap (findPath foundPaths (smallCave : path)) (nextCaves smallCave)

    nextCaves cave = HMap.lookup cave system |> fromMaybe []

part1 :: Text -> Maybe Int
part1 =
    parseInput
        .>> toCaveSystem
        .>> findAllPaths
        .>> length
