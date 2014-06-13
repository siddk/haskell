module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- Define a parser that recognizes symbols allowed in Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
