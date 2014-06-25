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
-- Lists
--          Lists in haskell are homogeneous - one type (i.e int)
--          A string in haskell is a list of chars
--          ++ operator combines lists --> problem: iterates
--             through entirety of list, very slow
--          : is the cons operator - instantaneous list creation
--            of the form (single value : list) --> appends
--            value to beginning of list
--          !! is the index operator list !! 6 --> 7th element
--          Lists are compared lexographically (1st element...)
--          head --> returns first element of list (car)
--          tail --> returns all but first element (cdr)
--          last --> returns last element of list
--          init --> returns all but last element
--          length --> returns length of list
--          null --> checks if list is empty
--          reverse --> reverses a list
--          take --> takes a number and a list, extracts number
--                   of elements from beginning of list
--          drop --> Drops n elements from list
--          maximum, minimum --> self-explanatory

