--Function that doubles a value
doubleMe x = x + x

--Function that takes two values, returns sum of doubling
--doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y --function composition