-- guards are a lot like pattern matching
-- also syntactic sugar for 'case'

foo n | n < 0     = "negative"   -- do this if 'n' is less than zero
      | n == 0    = "zero"       -- else do this if 'n' equals zero
      | otherwise = "positive"   -- else do this

{-
    Example output:
    
    *Main> foo (-1)
    "negative"
    
    *Main> foo 0
    "zero"
    
    *Main> foo 1
    "positive"
    *Main>     

-}
      