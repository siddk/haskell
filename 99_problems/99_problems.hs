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
myReverse :: [x] -> [x]
myReverse [] = []
myReverse lst = (myReverse (tail lst)) ++ ([(head lst)])

-- Problem 6: Find out whether a list is a palindrome.
--            A palindrome can be read forward or backward; e.g. (x a m a x)
isPalindrome :: (Eq x) => [x] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome list = head list == last list && isPalindrome (init $ tail list)

-- Problem 7: Flatten a list
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList lst -> [lst]
flatten (Elem listelem) = [listelem]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- Problem 8: