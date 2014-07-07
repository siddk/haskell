import

-- Input and Output in Haskell
-- Structure of an IO program:

-- f :: IO a
-- f = do
--   x <- action1
--   action2 x
--   y <- action3
--   action4 x y

-- In an IO block, each consecutive line will have an IO supertype

-- Problem 1: Ask a user to enter a list of numbers. Print the sum of the numbers
toList' :: String -> [Integer]
toList' input = read ("[" ++ input ++ "]")

main' = do
    putStrLn "Enter a list of numbers (separated by comma):"
    input <- getLine
    print $ sum (toList input)

-- Breakdown
-- putStrLn --> type IO
-- input <- getLine --> type IO String
-- print --> type IO

-- Continuous prompt, bind operator
maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing

getListFromString :: String -> Maybe [Integer]
getListFromString str = maybeRead $ "[" ++ str ++ "]"

askUser :: IO [Integer]
askUser =
    putStrLn "Enter a list of numbers (sep. by commas):" >>
    getLine >>= \input ->
    let maybeList = getListFromString input in
      case maybeList of
        Just l -> return l
        Nothing -> askUser

main :: IO ()
main = askUser >>=
  \list -> print $ sum list