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