-- Sum of Odd Elements
-- Return sum of odd elements from an list.

f arr = -- Fill up this function body

-- This part handles the Input/Output and can be used as it is. Do not change or modify it.
main = do
   inputdata <- getContents
   putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata