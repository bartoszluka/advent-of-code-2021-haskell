module Parsing where

import Relude.Unsafe (read)
import Text.Parsec (
    ParseError,
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

runParse :: Parser a -> String -> Either ParseError a
runParse p = parse p ""