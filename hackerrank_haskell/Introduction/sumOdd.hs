-- Sum of Odd Elements
-- Return sum of odd elements from an list.
f :: [Int] -> Int
f arr = sum ([x | x <- arr, odd x])

-- This part handles the Input/Output and can be used as it is. Do not change or modify it.
main = do
   inputdata <- getContents
   putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata