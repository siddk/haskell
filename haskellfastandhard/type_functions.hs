-- Declare type with functions, introduction to basic
-- function definition.

-- Function f (type declared) -> Sum squares of ints.
-- Problem: Parameters to f must be ints.
f :: Int -> Int -> Int
f x y = x*x + y*y

-- Function g (no type) -> Same as f
-- Type: undeclared, but type of function g is
-- g :: Num a => a -> a
-- a is a type variable
-- Num a is a type class for numbers --> contains basic
-- arithmetic operations, number operators.

-- g :: Num a => a -> a
g x y = x*x + y*y

-- Function h is the same as f and g, but with lambdas
-- Lambda is denoted \var = expression
-- Anonymous functions
h = \x y -> x*x + y*y

main = do
    print (f 2 3) -- f can only handle ints
    print (g 2.2 3) -- g can handle floats, ints, etc.