module Main where
import System.Environment

main :: IO ()
main = do
    putStrLn ("What is your name:")
    args <- getLine
    putStrLn ("Hello, " ++ args)
    -- putStrLn (show ((read (args !! 0)) + (read (args !! 1))))
