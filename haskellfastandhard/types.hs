-- Types
-- Type Rules:
-- type Name = AnotherType --> an alias, no compiler distinction
-- data Name = NameConstructor AnotherType --> does create a distinction
-- data --> can construct structures which can be recursives
-- deriving --> creates functions for you

-- 1. You can construct your own Types with aliases/type synonyms:
type Name  = String
type Color = String
showInfos :: Name ->  Color -> String
showInfos name color =  "Name: " ++ name
                        ++ ", Color: " ++ color
name :: Name
name = "Robin"

color :: Color
color = "Blue"

main = putStrLn $ showInfos name color
