-- Reverse List
-- Reverse a list without using reverse function. The input and output portions will be handled automatically. You need to write a function with the recommended method signature.

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]