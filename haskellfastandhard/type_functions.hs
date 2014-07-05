-- Declare type with functions, introduction to basic
-- function definition.

-- Function f -> Sum squares of two numbers
f :: Int -> Int -> Int
f x y = x*x + y*y

main = print (f 2 3)