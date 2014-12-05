-- Ref: Two Dozen Short Lessons in Haskell by Rex Page
-- Chapter 11 - Tuples

{-
    What if you wanted to compute the sequence of digits
    in a decimal number?
    
    The units digit of a non-negative decimal number is
    the remainder after the number is divided by 10
    
-}
unitsDigit :: Integral a => a -> a 
unitsDigit n = n `mod` 10       -- or  mod n 10

{- 
    Example:
        *Main> unitsDigit 1215
        5

    Note: the 'Integral' class covers the Integer and Int types
           and provides for whole-number division and remainder 
           operations.  It is the type returned by the 'mod' 
           function
           
           *Main> :t mod
            mod :: Integral a => a -> a -> a
           
           And the 'mod' function has a type constraint, so to
           must our 'unitsDigit' function.
           
           mod is the modulo function. To use a function as an
           operator we wrap it in back-ticks
           
    The trick to getting the tens digit is to drop the unit digit
    and extract the units digit of what's left
-}
tensDigit :: Integral a => a -> a
tensDigit n = d1
    where
        xSansLastDigit = n `div` 10
        d1 = xSansLastDigit `mod` 10

{-
    Example:
        *Main> tensDigit 1789
        8    
    
    The 'div' and 'mod' functions are used in combination so often
    that Haskell provides a 'divMod' function that returns the results
    of the 'div' and 'mod' parts together in a TUPLE.
    
    A tuple in Haskell is an aggregate of two or more
    individual components. The components may be
    of any type, and different items of a tuple may
    have different types. Tuples are denoted in Haskell
    scripts by a list of the components, separated by
    commas, with the entire list enclosed in parentheses.
    
    Equations that define variables as tuples can use a 'tuple pattern'
    to give each variable a name.
    
    Tuples, unlike lists, can hold values of different types.
    
    Example tuples:
        ("Rodney Bottoms", 2, True) :: (String, Int, Bool)
        (5,1) :: (Integer, Integer)
        
    We can extract the 100 digit by successive application of the
    'divMod' function
-}        
hundredsDigit n = d2
    where
        (xSansLastDigit, d0) = n `divMod` 10
        (xSansLast2Digits, d1) = xSansLastDigit `divMod` 10
        (xSansLast3Digits, d2) = xSansLast2Digits `divMod` 10
        
{-
    Example:
        *Main> hundredsDigit 1517
        5    
    
    Computed, roughly, as:
        hundredsDigit 1517
        ==> (xSansLastDigit, d0)    = 1517 `divMod` 10
        ==> (xSansLast2Digits, d1)  = 151  `divMod` 10
        ==> (xSansLast3Digits, d2)  = 15   `divMod` 10
        ==> (1, 5)
        ==> 5
-}
