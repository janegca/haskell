-- Pattern matching
--      functions may have multiple cases
--      each case is defined based on the 'pattern' of
--      the passed argument

fact 0 = 1                  -- match on an argument value of zero
fact n = n * fact (n-1)     -- match any non-zero argument value

len []  = 0                    -- match on an empty list
len lst = 1 + len (tail lst)   -- match any non-empty on

len' []     = 0                -- match on an empty list
len' (x:xs) = 1 + len xs       -- match on any list with at least
                               -- two elements; the second element,xs,
                               -- can be 1 element or an infinity
                               -- of elements

foo "hello" = "asdf"           -- match an argument of value "hello"
foo "world" = "BALLOONS"       -- match an argument of value "world"
foo _       = "CHUNKY BACON"   -- match any other argument

{-
    Example output:

    [1 of 1] Compiling Main   ( 09-pattern-matching.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> fact 4
    24
    
    *Main> len [1,2,3,4]
    4
    *Main> len' [1,2,3]
    3
    
    *Main> foo "hello"
    "asdf"
    *Main> foo "world"
    "BALLOONS"
    *Main> foo "goodbye"
    "CHUNKY BACON"
    *Main>     
    
-}
