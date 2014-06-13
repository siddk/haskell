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
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
         args <- getArgs
         putStrLn (readExpr (args !! 0))
