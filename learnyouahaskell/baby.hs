-- Function that doubles a value
doubleMe x = x + x

-- Function that takes two values, returns sum of doubling
doubleUs x y = doubleMe x + doubleMe y

-- If statement practice
doubleSmallNumber x = if x > 100
                        then x
                        else x*2

-- Basic Syntax Notes
-- let keyword: defines a name/value (variable) in ghci
--          i.e: let x = [3,4,5,6,7,9]
-- Lists in haskell are homogeneous - one type (ints, chars)
-- A string in haskell is a list of chars
