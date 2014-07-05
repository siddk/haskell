-- Declare type with functions, introduction to basic
-- function definition.

-- Function f (type declared) -> Sum squares of ints.
-- Problem: Parameters to f must be ints.
f :: Int -> Int -> Int
f x y = x*x + y*y

-- Function g (no type) -> Same as f
g x y = x*x + y*y

main = do
    print (f 2 3) -- f can only handle ints
    print (g 2.2 3) -- g can handle floats, ints, etc.