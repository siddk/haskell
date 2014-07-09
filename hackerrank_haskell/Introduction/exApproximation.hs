-- Evaluation e^x
-- The series expansion of ex is given by:
-- 1 + x + x2/2! + x3/3! + x4/4! + .......
-- Evaluate e^x for given values of x, by using the above expansion for the first 10 terms.

solve :: Double -> Double
solve x = -- Insert your code here --

main :: IO ()
main = getContents >>= mapM_ print. map solve. map (read::String->Double). tail. words