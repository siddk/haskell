-- List Replication
-- Given a list repeat each element of the list n times. The input and output portions will be handled automatically by the grader. You need to write a function with the recommended method signature

f n arr = -- Complete this Function

-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main = do
   n <- readLn :: IO Int
   inputdata <- getContents
   mapM_ putStrLn $ map show $ f n $ map (read :: String -> Int) $ lines inputdata