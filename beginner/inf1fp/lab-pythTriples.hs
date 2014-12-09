-- Informatics 1 - Functional Programming
-- Lab Week Exercise - Pythagorean Triples
--      Define given functions

import Test.QuickCheck


{-
    A Pythagorean triple is a set of three integers (a,b,c)
    which satisfy the equation a^2 + b^2 = c^2.
    
    For this exercise, don't worry about negative or zero
    lengths.
    
    Example:
        *Main> isTriple 3 4 5
        True
        *Main> isTriple 3 4 6
        False
    
-}

square :: Int -> Int
square x = x * x

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = ((square a) + (square b)) == (square c)

{-
    A simple formula for finding Pythagorean triples is:
    (x^2 - y^2, 2yx, x^2 + y^2) is a Pythagorean triple for
    all positive integers x y with x > y.
    
    For this exercise, forget about ensuring x and y are
    positive or x > y.
    
    Example:
        *Main> leg1 5 4
        9
        *Main> leg2 5 4
        40
        *Main> hyp 5 4
        41    
-}

leg1 :: Int -> Int -> Int
leg1 x y = (square x) - (square y)

leg2 :: Int -> Int -> Int
leg2 x y = 2 * y * x

hyp :: Int -> Int -> Int
hyp x y = (square x) + (square y)

{-
    QuickCheck is a Haskell utility that, given a function, will
    create random data with which to test it.
    
    QuickCheck recognizes functions that begin with 'prop_' (for
    property).  
    
    The prop_triple function uses the above formulae to generate
    and test for Pythagorean triples. If it fails, there is an
    error in one or more of the above.
    
    QuickCheck can be run from within ghci
    
    Example:
        *Main> quickCheck prop_triple
        +++ OK, passed 100 tests.
    
-}

prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

