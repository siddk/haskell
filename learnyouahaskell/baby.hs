--Function that doubles a value
doubleMe x = x + x

--Function that takes two values, returns sum of doubling
doubleUs x y = doubleMe x + doubleMe y

--If statement practice
doubleSmallNumber x = if x > 100
                        then x
                        else x*2

