-- Hello You!
-- Hello World with IO --> Reads in line
main = do
    print "What is your name?"
    name <- getLine
    print ("Hello" ++ "name" ++ "!")