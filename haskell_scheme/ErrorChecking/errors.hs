module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error

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

--Helper function for showing lists
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

--Define print formatting for LispVal values
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

--Make haskell default Show an instance of showVal
instance Show LispVal where show = showVal

--Setup primitive type eval
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

--Setup apply function, to apply a function to a series of arguments --> Primitive functionality
-- i.e. (+ 2 2)
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

--Define list of primitives
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

--Define numericBinop
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

--Define unpackNum
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

--Create function readExpr, passes input string to Parsec Parse function,
--which takes a Parser (parseExpr), a name for the input ("Lisp")
--and the input itself (input).
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

--Create Datatype to represent Errors
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- Main function, reads in command line args, executes readExpr on args
main :: IO ()
main = getArgs >>= print . eval . readExpr . head
