-- Hello Worlds
-- Given n, print "Hello World" n times
import Control.Monad
hello_worlds n = replicateM_ n (putStrLn "Hello World")

-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
   n <- readLn :: IO Int
   hello_worlds n
