module Parsing where

import Text.Parsec
  ( ParseError,
    char,
    digit,
    eof,
    many1,
    parse,
    sepBy,
    string,
  )
import Text.Parsec.String (Parser)

integer :: Parser Int
integer = do
  n <- many1 digit
  return (read n)

parseInput :: Parser [Int]
parseInput = sepBy integer (char ',')

runParse :: Parser a -> String -> Either ParseError a
runParse p = parse p ""