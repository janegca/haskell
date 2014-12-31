-- another form of pattern matching
--
foo n = case signum n of             -- if (signum n) is
            (-1) -> "negative"       -- minus 1, do this
            0    -> "zero"           -- else if it is 0, do this
            1    -> "positive"       -- else if it is 1, do this
            
{-
    Example:
    
    [1 of 1] Compiling Main             ( 12-case.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> foo (-3)
    "negative"
    *Main> foo 0
    "zero"
    *Main> foo 3
    "positive"
    *Main>     

-}            