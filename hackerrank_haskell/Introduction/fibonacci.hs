-- Fibonacci Series
-- The Fibonacci series begins with 0 and 1 (which are the first and the second terms respectively). After this, every
-- element is the sum of the preceding elements.
-- Given the starter code, complete the fibonacci function to return the Nth term.
module Main where

fib :: Int -> Int
fib n = fib2 0 1 n
    where fib2 a b n = if n == 1
                       then a
                       else fib2 b (a + b) (n - 1)


-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
    input <- getLine
    print . fib . (read :: String -> Int) $ input