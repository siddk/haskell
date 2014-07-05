-- even_sum.hs
-- Problem: Given a list of integers, return the sum of the even numbers in the list.

-- Version 1
evenSum :: [Integer] -> Integer
evenSum l = accumSum 0 l
accumSum n l = if l == []
                then n
                else let x = head l
                         xs = tail l
                     in if even x
                        then accumSum (n+x) xs
                        else accumSum n xs

main = do
    print (evenSum [1, 2, 3, 4, 5])