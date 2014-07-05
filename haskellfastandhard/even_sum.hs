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

-- Version 2
-- Type class generalization, where, let helper functions
evenSum' :: Integral a => [a] -> a
evenSum' l = accumSum 0 l
    where accumSum n l =
            if l == []
                then n
                else let x = head l
                         xs = tail l
                     in if even x
                        then accumSum (n+x) xs
                        else accumSum n xs

-- Version 3
-- Pattern matching, replace null check, list composition
evenSum'' :: Integral a => [a] -> a
evenSum'' l = accumSum 0 l
    where
        accumSum n [] = n
        accumSum n (x:xs) = if even x
                            then accumSum (n+x) xs
                            else accumSum n xs

-- Version 4
-- n reduction -> remove l from function
evenSum''' :: Integral a => [a] -> a
evenSum''' = accumSum 0
    where
        accumSum n [] = n
        accumSum n (x:xs) = if even x
                            then accumSum (n+x) xs
                            else accumSum n xs

main = do
    print (evenSum [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    print (evenSum' [1..10])
    print (evenSum'' [1..10])
    print (evenSum''' [2, 4..10])

