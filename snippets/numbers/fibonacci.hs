-- fibonacci numbers

{-  
    References:
        [HR]    The Haskell Road to Logic, Math and Programming

-}
-- returns the nth fibonacci number
fib :: (Num b, Num a, Eq a) => a -> b
fib n = fib2 0 1 n where
    fib2 a b 0 = a
    fib2 a b n = fib2 b (a+b) (n-1)