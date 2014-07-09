-- Filter Array
-- Filter a given array of integers and leave only that values which are less than a specified value X. The integers in the output should be in the same sequence as they were in the input.

f :: Int -> [Int] -> [Int]
f n arr = --Fill up this function

-- The Input/Output section. You do not need to change or modify this part
main = do
    n <- readLn :: IO Int
    inputdata <- getContents
    let
        numbers = map read (lines inputdata) :: [Int]
    putStrLn . unlines $ (map show . f n) numbers