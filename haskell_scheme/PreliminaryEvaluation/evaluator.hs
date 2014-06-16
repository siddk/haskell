module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

-- Define a parser that recognizes symbols allowed in Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--Define a parser that ignores whitespace ... uses skipMany1 Parser action
spaces :: Parser ()
spaces = skipMany1 space

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
                char '\"'
                x <- many (noneOf "\"")
                char '\"'
                return $ String x

--Create LispVal Parser to parse out Atoms. An atom is a letter or symbol
--followed by any number of letters, digits, or symbols
parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

--Create LispVal Parser to parse out Numbers. Uses liftM to lift Parser
--String monad off of many1 digit
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

--Create LispVal Parser for parenthesized lists.
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

--Create LispVal Parser for dotted lists.
parseDottedList :: Parser LispVal
parseDottedList = do
                    head <- endBy parseExpr spaces
                    tail <- char '.' >> spaces >> parseExpr
                    return $ DottedList head tail

--Create LispVal Parser for Quoted datatypes
parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]

--Create general LispVal Expression parser
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

--Create function readExpr, passes input string to Parsec Parse function,
--which takes a Parser (parseExpr), a name for the input ("Lisp")
--and the input itself (input).
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

--Define print formatting for LispVal values
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents

-- Main function, reads in command line args, executes readExpr on args
main :: IO ()
main = do
         args <- getArgs
         putStrLn (readExpr (args !! 0))
