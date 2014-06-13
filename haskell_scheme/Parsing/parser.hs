module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- Define a parser that recognizes symbols allowed in Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Create function readExpr, passes input string to Parsec Parse function,
-- which takes a Parser (symbol), a name for the input ("Lisp") and the
-- input itself (input).
readExpr :: String -> String
