-- Update List
-- Update the values of a list with their absolute values. The input and output portions will be handled automatically during grading. You only need to write a function with the recommended method signature.

-- Enter your code here. Read input from STDIN. Print output to STDOUT

f arr = -- Complete this function here

-- This section handles the Input/Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata