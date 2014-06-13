module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- Define a parser that recognizes symbols allowed in Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--Define a parser that ignores whitespace ... uses skipMany1 Parser action
spaces :: Parser ()
spaces = skipMany1 space

-- Create function readExpr, passes input string to Parsec Parse function,
-- which takes a Parser (spaces >> symbol), a name for the input ("Lisp") -- and the input itself (input).
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

--Define a new data type that can hold a Lisp (Scheme) value, with |
--separated constructors.
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

--Create LispVal Parser to parse out Strings --> designated by ""
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

--Create LispVal Parser to parse out Atoms. An atom is a letter or symbol
--followed by any number of letters, digits, or symbols
parseAtom :: Parser LispVal
parseAtom = do
              

-- Main function, reads in command line args, executes readExpr on args
main :: IO ()
main = do
         args <- getArgs
         putStrLn (readExpr (args !! 0))
