
module Utils where

(!?!) :: [a] -> Int -> Maybe a
[] !?! _ = Nothing
_ !?! n | n < 0 = Nothing
(x:xs) !?! 0 = Just x
(x:xs) !?! n = xs !?! (n - 1)

infixl 9 !?!

setByIndex :: Int -> a -> [a] -> [a]
setByIndex n x xs = [if i == n then x else y | (y, i) <- zip xs [0..] ]
