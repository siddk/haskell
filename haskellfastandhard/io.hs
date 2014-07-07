-- Input and Output in Haskell
-- Structure of an IO program:

-- f :: IO a
-- f = do
--   x <- action1
--   action2 x
--   y <- action3
--   action4 x y