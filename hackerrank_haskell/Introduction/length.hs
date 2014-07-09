-- List Length
-- Count the number of elements in an array without using count, size or length operators (or their equivalents). The input and output portions will be handled automatically by the grader. You only need to write a function with the recommended method signature.

len :: [Int] -> Int
len [] = 0
len arr = 1 + (len (tail arr))