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
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

--Setup apply function, to apply a function to a series of arguments --> Primitive functionality
-- i.e. (+ 2 2)
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

--Define list of primitives
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">;", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))]

--Define numericBinop
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

--Define unpackNum
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

--Create Datatype to represent Errors
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

--Function to show errors
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

--Bind show to showError
instance Show LispError where show = showError

--Make error type an instance of Error
instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default
type ThrowsError = Either LispError

--Helper functions
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

--Create function readExpr, passes input string to Parsec Parse function,
--which takes a Parser (parseExpr), a name for the input ("Lisp")
--and the input itself (input).
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

-- Main function, reads in command line args, executes readExpr on args
main :: IO ()
main = do
         args <- getArgs
         evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
         putStrLn $ extractValue $ trapError evaled