module Parsing where

import Relude.Unsafe (read)
import Text.Parsec (
    ParseError,
    digit,
    many1,
    parse,
 )
import Text.Parsec.String (Parser)

integer :: Parser Int
integer = do
    n <- many1 digit
    return (read n)

runParse :: Parser a -> String -> Either ParseError a
runParse p = parse p ""