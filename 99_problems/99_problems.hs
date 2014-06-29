-- 99 Haskell Problems, based on 99 Prolog Probems and 99 LISP Problems
-- 99_problems.hs contains problem statements, and solution scripts (using standard lib)

-- Problem 1: Find the last element of a list.
myLast :: [x] -> x
myLast x = last x

-- Problem 2: Find the last but one element of a list.
myButLast :: [x] -> x
myButLast x = last $ init x

-- Problem 3: Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [value] -> Int -> value
elementAt list num = list !! (num - 1)

-- Problem 4: Find the number of elements of a list.
myLength :: [x] -> Int
myLength x = length x

-- Problem 5: Reverse a list.
myLength :: [x] -> [x]
myLength list = head list : reverse $ tail list