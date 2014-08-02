-- Filter odd indices from list
-- For a given list with N integers, return a new list containing the elements from odd indices.

f :: [a] -> [a]
f (x:xs) = filterOdd xs True

filterOdd :: [a] -> Bool -> [a]
filterOdd [] bool = []
filterOdd (x:xs) bool = if bool
                        then x : (filterOdd xs (not bool))
                        else filterOdd xs (not bool)

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata
