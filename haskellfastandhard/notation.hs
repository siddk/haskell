-- Notation.hs
-- Basic type notation, data structures


-- Arithmetic
-- Follows order of operations, use parentheses


-- Logic
-- True || False ⇒ True
-- True && False ⇒ False
-- True == False ⇒ False
-- True /= False ⇒ True  (/=) => not equal (diff)


-- Powers
-- x^n     for n an integral (understand Int or Integer)
-- x**y    for y any kind of number (Float for example)


-- Int vs. Integer
-- Int is more efficient, but it is capped
-- Integer is unbounded (i.e. factorial 1000)


-- Lists, List creation
-- Lists have to be singular type
-- Can create a list with patterns, comprehensions
--
-- []                      ⇔ empty list
-- [1,2,3]                 ⇔ List of integral
-- ["foo","bar","baz"]     ⇔ List of String
-- 1:[2,3]                 ⇔ [1,2,3], (:) => cons
-- 1:2:[]                  ⇔ [1,2]
-- [1,2] ++ [3,4]          ⇔ [1,2,3,4], (++) concatenate
-- [1,2,3] ++ ["foo"]      ⇔ ERROR String ≠ Integral
-- [1..4]                  ⇔ [1,2,3,4]
-- [1,3..10]               ⇔ [1,3,5,7,9]
-- [2,3,5,7,11..100]       ⇔ ERROR! I am not so smart!
-- [10,9..1]               ⇔ [10,9,8,7,6,5,4,3,2,1]


-- Strings
-- Strings are a list of Char
--
-- 'a' :: Char
-- "a" :: [Char]
-- ""  ⇔ []
-- "ab" ⇔ ['a','b'] ⇔  'a':"b" ⇔ 'a':['b'] ⇔ 'a':'b':[]
-- "abc" ⇔ "ab"++"c"
--
-- In real code, use Data.Text to represent strings


-- Tuples
-- Unlike lists, tuples can have multiple types
-- A special type of tuple is the pair (a, b)
-- Pairs have special functions: fst, snd
-- Otherwise, index with !!


-- Function Composition
-- Two operators -> $, .
-- By default:
-- f g h x         ⇔  (((f g) h) x)
-- $ delays function evaluation to the right
-- f g $ h x       ⇔  f g (h x) ⇔ (f g) (h x)
-- f $ g h x       ⇔  f (g h x) ⇔ f ((g h) x)
-- f $ g $ h x     ⇔  f (g (h x))
-- (.) the composition function
-- (f . g) x       ⇔  f (g x)
-- (f . g . h) x   ⇔  f (g (h x))


-- Function declaration
-- Defining the type of function before declaration isn't necessary, but it is considered good practice.

-- x :: Int           ⇔ x is of type Int
-- x :: a             ⇔ x can be of any type
-- x :: Num a => a    ⇔ x can be any type a in Num type class
-- f :: a -> b        ⇔ f is a function from a to b
-- f :: a -> b -> c   ⇔ f is a function from a to (b→c)
-- f :: (a -> b) -> c ⇔ f is a function from (a→b) to c

-- Infix notation, multiple function definitions:
square :: Num a => a -> a
square x = x^2 -- ^ uses infix notation, has prefix form
square' x = (^) x 2 -- ' signifies a variation of function
square'' x = (^2) x -- ^ has multiple prefix forms
square''' = (^2) -- remove x, n-reduction

-- Multiple absolute value functions, with conditionals
absolute :: (Ord a, Num a) => a -> a -- a in Ord, Num type classes
absolute x = if x >= 0 then x else -x
absolute' x
    | x >= 0 = x
    | otherwise = -x

main = do
    print (square 4)
    print (square' 4)
    print (square'' 4)
    print (square''' 4)
    print (absolute (-22))
    print (absolute' (-22))




