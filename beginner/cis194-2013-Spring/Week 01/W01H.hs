{-# OPTIONS_GHC -Wall #-}

module W01H where

{-
    Ref: http://www.seas.upenn.edu/%7Ecis194/spring13/hw/01-intro.pdf
-}

-- -----------------------------------------------------------------------
-- Credit Card Validation
-- -----------------------------------------------------------------------
{-

    Exercise 1 

    We need to first find the digits of a number. Define the functions
        toDigits :: Integer -> [Integer]
        toDigitsRev :: Integer -> [Integer]

    toDigits should convert positive Integers to a list of digits. 
    (For 0 or negative inputs, toDigits should return the empty list.) 
    
    toDigitsRev should do the same, but with the digits reversed.
    
    Example: toDigits 1234 == [1,2,3,4]
    Example: toDigitsRev 1234 == [4,3,2,1]
    Example: toDigits 0 == []
    Example: toDigits (-17) == []

-}
-- convert a positive integer to a list of digits
toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

-- convert a positive integer to a list of digits in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
toDigitsRev n = mod n 10 : toDigitsRev (div n 10)

ex1a, ex1b, ex1c, ex1d, ex1test :: Bool
ex1a = toDigits 1234    == [1,2,3,4]
ex1b = toDigitsRev 1234 == [4,3,2,1]
ex1c = toDigits 0       == []
ex1d = toDigits (-17)   == []
ex1test = ex1a && ex1b && ex1c && ex1d

{-
    Exercise 2 
    
    Once we have the digits in the proper order, we need to
    double every other one. Define a function
    
        doubleEveryOther :: [Integer] -> [Integer]
    
    Remember that doubleEveryOther should double every other number
    beginning from the right, that is, the second-to-last, fourth-to-last,
    . . . numbers are doubled.
    
    Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
    Example: doubleEveryOther [1,2,3] == [1,4,3]

-}

-- double every other digit, moving right to left
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = dbl (reverse n)
    where   
        dbl :: [Integer] -> [Integer]
        dbl []       = []
        dbl (x:[])   = [x]
        dbl (x:y:zs) = dbl zs ++ (2*y) : [x]

ex2a, ex2b, ex2test :: Bool        
ex2a = doubleEveryOther [8,7,6,5] == [16,7,12,5]
ex2b = doubleEveryOther [1,2,3] == [1,4,3]
ex2test = ex2a && ex2b

{-
    Exercise 3 
    The output of doubleEveryOther has a mix of one-digit
    and two-digit numbers. Define the function
    
    sumDigits :: [Integer] -> Integer
    
    to calculate the sum of all digits.
    
    Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
-}

-- sums all the digits in a list of integers
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) | x > 9     = sum (toDigits x) + sumDigits xs
                 | otherwise = x + sumDigits xs
    
ex3test :: Bool    
ex3test = sumDigits [16,7,12,5] == 22 

{-
Exercise 4 

    Define the function
        
        validate :: Integer -> Bool
    
    that indicates whether an Integer could be a valid credit card number.
    This will use all functions defined in the previous exercises.
    
    Example: validate 4012888888881881 = True
    Example: validate 4012888888881882 = False

-}

-- validate a credit card number
validate :: Integer -> Bool
validate ccn = mod n 10 == 0
    where n = (sumDigits . doubleEveryOther . toDigits) ccn
        
ex4a, ex4b, ex4test :: Bool
ex4a = validate 4012888888881881 == True
ex4b = validate 4012888888881882 == False
ex4test = ex4a && ex4b

ccnTest :: Bool
ccnTest = ex1test && ex2test && ex3test && ex4test

-- ----------------------------------------------------------------------- 
-- Towers of Hanoi
-- -----------------------------------------------------------------------
{-

    Exercise 5 

    The Towers of Hanoi is a classic puzzle with a solution
    that can be described recursively. Disks of different sizes are stacked
    on three pegs; the goal is to get from a starting configuration with
    all disks stacked on the first peg to an ending configuration with all
    disks stacked on the last peg, as shown in Figure 1.

    The only rules are
    • you may only move one disk at a time, and
    • a larger disk may never be stacked on top of a smaller one.
    
    For example, as the first move all you can do is move the topmost,
    smallest disk onto a different peg, since only one disk may be moved
    at a time.
    
    From this point, it is illegal to move to the configuration shown in 
    Figure 3, because you are not allowed to put the green disk on top of
    the smaller blue one.
    
    To move n discs (stacked in increasing size) from peg a to peg b
    using peg c as temporary storage,
    
    1. move n - 1 discs from a to c using b as temporary storage
    2. move the top disc from a to b
    3. move n - 1 discs from c to b using a as temporary storage.
    
    For this exercise, define a function hanoi with the following type:
    
        type Peg = String
        type Move = (Peg, Peg)
        hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
    
    Given the number of discs and names for the three pegs, hanoi
    should return a list of moves to be performed to move the stack of
    discs from the first peg to the second.

    Note that a type declaration, like type Peg = String above, makes
    a type synonym. In this case Peg is declared as a synonym for String,
    and the two names Peg and String can now be used interchangeably.
    Giving more descriptive names to types in this way can be used to
    give shorter names to complicated types, or (as here) simply to help
    with documentation.

    Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-}

type Peg = String
type Move = (Peg, Peg)

-- give a list of the moves necessary to move n discs from the
-- first peg to the last, discs are stacked in increasing size
-- 's'ource, 'd'estination, 't'emporary pegs
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []      -- no more discs in stack
hanoi n s d t = hanoi (n-1) s t d 
             ++ ((s,d) : hanoi (n-1) t d s)