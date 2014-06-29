-- 99 Haskell Problems, based on 99 Prolog Probems and 99 LISP Problems
-- 99_problems.hs contains problem statements, and solution scripts

-- Problem 1: Find the last element of a list.
myLast :: [x] -> x
myLast x = last x

-- Problem 2: Find the last but one element of a list.
myButLast :: [x] -> x
myButLast x = last $ init x