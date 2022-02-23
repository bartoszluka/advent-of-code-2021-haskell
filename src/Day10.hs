module Day10 (part1, part2) where

import Extra (choose, median, toMaybe)
import Stack (
    PoppedError (PoppedWrongItem),
    StackOrError,
 )
import qualified Stack (
    popIfEquals,
    push,
 )
import qualified Text.Show

data BracketStyle
    = Square
    | Squigly
    | Round
    | Angle
    deriving (Show, Eq)

data OpenOrClose = Open | Close deriving (Show, Eq)

data Bracket = Bracket BracketStyle OpenOrClose deriving (Eq)

instance Show Bracket where
    show (Bracket Round Open) = "("
    show (Bracket Round Close) = ")"
    show (Bracket Square Open) = "["
    show (Bracket Square Close) = "]"
    show (Bracket Squigly Open) = "{"
    show (Bracket Squigly Close) = "}"
    show (Bracket Angle Open) = "<"
    show (Bracket Angle Close) = ">"

toBracket :: Char -> Maybe Bracket
toBracket =
    \case
        '(' -> Just <| Bracket Round Open
        '{' -> Just <| Bracket Squigly Open
        '[' -> Just <| Bracket Square Open
        '<' -> Just <| Bracket Angle Open
        ')' -> Just <| Bracket Round Close
        '}' -> Just <| Bracket Squigly Close
        ']' -> Just <| Bracket Square Close
        '>' -> Just <| Bracket Angle Close
        _ -> Nothing

parse :: Text -> Maybe [Bracket]
parse = toString .> traverse toBracket

scanLine :: [Bracket] -> [StackOrError Bracket]
scanLine = scanl popOrPush (Right mempty)
  where
    popOrPush stackOrError (Bracket style Open) = Stack.push (Bracket style Close) <$> stackOrError
    popOrPush stackOrError bracket = stackOrError >>= Stack.popIfEquals bracket

corruptedLineScore :: [Bracket] -> Int
corruptedLineScore = scanLine .> viaNonEmpty last .=> extractError .>> syntaxErrorScore .> fromMaybe 0
  where
    extractError :: StackOrError Bracket -> Maybe BracketStyle
    extractError (Left (PoppedWrongItem (Bracket style _) _)) = Just style
    extractError _ = Nothing

    syntaxErrorScore :: BracketStyle -> Int
    syntaxErrorScore = \case
        Round -> 3
        Square -> 57
        Squigly -> 1197
        Angle -> 25137

part1 :: Text -> Int
part1 = lines .> choose (parse .>> corruptedLineScore) .> sum

incompleteLineScore :: [Bracket] -> Maybe Int
incompleteLineScore = scanLine .> viaNonEmpty last .=> toMaybe .>> toList .>> errorScore
  where
    errorScore :: [Bracket] -> Int
    errorScore = foldl' (\accum current -> accum * 5 + syntaxErrorScore current) 0

    syntaxErrorScore :: Bracket -> Int
    syntaxErrorScore = \case
        Bracket Round _ -> 1
        Bracket Square _ -> 2
        Bracket Squigly _ -> 3
        Bracket Angle _ -> 4

part2 :: Text -> Int
part2 = lines .> choose (parse .=> incompleteLineScore) .> median
