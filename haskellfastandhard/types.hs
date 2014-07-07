-- Types
-- Type Rules:
-- type Name = AnotherType --> an alias, no compiler distinction
-- data Name = NameConstructor AnotherType --> does create a distinction
-- data --> can construct structures which can be recursives
-- deriving --> creates functions for you
import Data.List

-- You can construct your own Types with aliases/type synonyms:
type AliasName  = String
type AliasColor = String
aliasShowInfos :: AliasName ->  AliasColor -> String
aliasShowInfos name color =  "Name: " ++ name
                        ++ ", Color: " ++ color
aliasName :: AliasName
aliasName = "Robin"

aliasColor :: AliasColor
aliasColor = "Blue"

aliasMain = putStrLn $ aliasShowInfos aliasName aliasColor
-- The issue with this is that you can swap around parameters, or pass in Strings without declaring them as type Name or Color, and the program will compile and run perfectly.

-- To remedy this, you can create own types with data keyword
data DataName   = DataNameConstr String
data DataColor  = DataColorConstr String

dataShowInfos :: DataName ->  DataColor -> String
dataShowInfos (DataNameConstr name) (DataColorConstr color) =
      "Name: " ++ name ++ ", Color: " ++ color

dataName  = DataNameConstr "Robin"
dataColor = DataColorConstr "Blue"

dataMain = putStrLn $ dataShowInfos dataName dataColor

-- Syntax of the 'data' keyword:
-- data TypeName =   ConstructorName  [types]
--                | ConstructorName2 [types]
--                | ...

-- Or, to declare fields
-- data DataTypeName = DataConstructor {
--                      field1 :: [type of field1]
--                    , field2 :: [type of field2]
--                    ...
--                    , fieldn :: [type of fieldn] }

-- This second manner is better, as it allows one to do the following:
data Complex a = Complex { real :: a, img :: a }
c = Complex 1.0 2.0
z = Complex { real = 3, img = 4 }
-- real c ⇒ 1.0
-- img z ⇒ 4

-- Recursive Types
-- Standard Example: Binary Trees
data BinTree a = Empty
               | Node a (BinTree a) (BinTree a) deriving (Show)

-- deriving creates the mentioned functions for the class. In this case, deriving creates the Show (to_string) function for the binary tree node class

-- Function to create a tree from a list:
-- Creates a tree where first element is the tree node, all elements of list less than first element are on left side, rest are on right side
treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))
