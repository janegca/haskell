-- using closures to make "objects"
--
-- [Note: this is a 'lambda', the function, cmd, 'closes' over
--        the 'a' variable, remembering it's value from the
--        environment in which the 'cmd function is created]
num a = \cmd b -> case cmd of
                     "plus" -> a + b
                     "minus" -> a - b
                     "times" -> a * b
                     "divided by" -> a / b

{- gchi session: -------------

*Main> let three = num 3
*Main> three "plus" 2
5.0
*Main> three "times" 11
33.0

------------------ -} 

{-
    Example
    
    *Main> let three = num 3
    *Main> let four = num 4
    *Main> three "plus" 2
    5.0
    *Main> three "times" 11
    33.0
    *Main> four "plus" 2
    6.0
    *Main> four "times" 11
    44.0    

-}