-- Definite Integral
-- Using equal Sub-Intervals of length = 0.001, you need to
-- Evaluate the area bounded by a given polynomial function of the kind described above, between given limits a and b.
-- Evaluate the volume of the solid obtained by revolving this polynomial curve around the X-Axis.
-- Round off your answers to one decimal place(x.x). An error margin of +/- 0.2 will be tolerated.

import Text.Printf (printf)

-- This function should return a list [area, volume].
--solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [defIntegral l r a b] ++ [volSolid l r a b]

defIntegral [] [] a b = if a < b
                        then defIntegral

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
