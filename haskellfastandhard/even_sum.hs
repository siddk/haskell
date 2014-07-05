-- even_sum.hs
-- Problem: Given a list of integers, return the sum of the even numbers in the list.

import Data.List(foldl') -- for foldl'

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

-- Version 5
-- Higher order functions (filter, map, foldl)
evenSum'''' :: Integral a => [a] -> a
evenSum'''' l = mysum 0 (filter even l)
    where
        mysum n [] = n
        mysum n (x:xs) = mysum (n+x) xs

-- Version 6
-- foldl is haskell's accumulate
-- foldl is lazy, doesn't evaluate accumulator function
-- foldl' is strict, but needs external import
evenSum''''' :: Integral a => [a] -> a
evenSum''''' l = foldl' mysum 0 (filter even l)
    where mysum acc value = acc + value

-- Version 7
-- Lambda expression for accumulator
evenSum'''''' :: Integral a => [a] -> a
evenSum'''''' l = foldl' (\x y -> x + y) 0 (filter even l)

-- Version 8
-- Of course, (\x y -> x + y) is just (+)
evenSum''''''' :: Integral a => [a] -> a
evenSum''''''' l = foldl' (+) 0 (filter even l)

-- Version 9
-- n-reduction, this is more flexible
evenSum2 :: Integral a => [a] -> a
evenSum2 = (foldl' (+) 0) . (filter even)

-- squareEvenSum
-- Uses version 9 to get the sum of all even squares of a list
squareEvenSum :: Integral a => [a] -> a
squareEvenSum = evenSum2 . (map (^2))

main = do
    print (evenSum [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    print (evenSum' [1..10])
    print (evenSum'' [1..10])
    print (evenSum''' [2, 4..10])
    print (evenSum'''' [1..10])
    print (evenSum''''' [1..10])
    print (evenSum'''''' [1..10])
    print (evenSum''''''' [1..10])
    print (evenSum2 [1..10])
    print (squareEvenSum [1..10])

