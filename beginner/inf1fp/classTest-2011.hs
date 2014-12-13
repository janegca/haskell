-- Informatics 1 - Functional Programming 
-- 2011 Class Test
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/exams/classexam-2011.pdf
--
-- Instructions state any function from the Stanard Prelude plus the
-- following libraries may be used.
--
import Test.QuickCheck

import Data.Char
import Data.List
import Data.Maybe   -- this hasn't been covered in 2014 lectures 1-9

{- 1.
    
   (a)  Write a function 
            f :: Char -> Int 
        that converts a hexadecimal digit to its value. 
        
        A hexadecimal digit is one of the characters '0' through '9' or 
        'a' through 'f'. The values of the digits '0' through '9' are the 
        integers 0 through 9, and the value of the letters 'a' through 'f' 
        are the numbers 10 through 15. Upper and lower case letters have 
        the same value. For example,
        
            f '0' == 0 f 'a' == 10 f 'A' == 10
            f '2' == 2 f 'c' == 12 f 'C' == 12
            f '9' == 9 f 'f' == 15 f 'F' == 15

        For any character that is not a hexadecimal digit, f should return 
        an error.
        
        [Note: provided solution does not use thee library functions
               uses guards to check for the ord values of characters.
               A second alternative solution builds map and lookup
               functions.]
-}
f :: Char -> Int
f ch 
    | isHexDigit ch = digitToInt ch
    | otherwise     = error ("not a hexidecimal digit")
    
{-
    (b) Using f, define a function 
            g :: String -> Int 
            
        that given a string returns the maximum value of any hexadecimal
        digit in the string, ignoring any character that is not a 
        hexadecimal digit If the string contains no digit or letter it 
        should return -1. For example,
            g "3142" == 4 g "a2cz!" == 12 g "" == -1
-}    
g :: String -> Int
g [x] = if isHexDigit x then f x else -1
g str = foldr max (-1) [ (f x) `max` (f y) | (x,y) <- zip str (tail str)
                                           , isHexDigit x
                                           , isHexDigit y]

-- provided solution, much cleaner
--   'isHexDigit' replaces a custom 'isHex' function that was
--   written as part of the provided solution to 1.a
g' :: String -> Int
g' xs = maximum (-1 : [ f x | x <- xs, isHexDigit x ])
                                                                                                   
{-
    (c) Again using f, define a function,
            h :: String -> Int 
            
        that behaves identically to g, this time using basic functions 
        and recursion,  but not list comprehension or library functions.

        OK - blew this as my 'g' is recursive. How to do 'g' with
        list comprehension??  Figured it out and swapped answeres aroudn.
-}
h :: String -> Int
h str = maxVal str (-1)
    where
        maxVal :: String -> Int -> Int
        maxVal (x:xs) res |   (isHexDigit x)
                            &&( f x) > res   = maxVal xs (f x)
                          | otherwise        = maxVal xs res
        maxVal _ res = res
        
-- provided solution, again, much cleaner
-- THINK FUNCTIONALLY - in terms of WHAT the values are, 
--                      NOT how you want to manipulate them
--                      in terms of RESULTS, NOT PROCESS (??)
-- REMEMBER - recursion is like induction, assume the the function
--            you're trying to write ALREADY does what you want it
--            to do
h' :: String -> Int
h' [] = -1
h' (x:xs) | isHexDigit x  =  f x `max` h xs
          | otherwise     =  h xs
        
{-
    2.
    
    (a) Write a function 
            c :: [Int] -> Int 
        
        that returns the product of the difference of successive elements 
        in the list. If the list has one element the function should 
        return one, and if the list is empty it should indicate an error.
        
        c [3,1,4,2,5] = (3-1) * (1-4) * (4-2) * (2-5) = 36
        c [2,4,6,8] = (2-4) * (4-6) * (6-8) = -8
        c [1,2,2,1] = (1-2) * (2-2) * (2-1) = 0
        c [-1,2,-3,4] = ((-1)-2) * (2-(-3)) * ((-3)-4) = 105
        c [42] = 1
        c [] = error
        
        Your definition may use basic functions, list comprehension, 
        and library functions, but not recursion.
-}
c :: [Int] -> Int
--c []    = error ("empty list")    -- these aren't needed
--c [x]   = 1
c lst   = product [ (x - y) | (x,y) <- zip lst (tail lst) ]

{-
    (b) Define a second function, 
            d :: [Int] -> Int 
            
        that behaves identically to c, this time using basic functions 
        and recursion, but not list comprehension or other library 
        functions.
-}
d :: [Int] -> Int
d []  = error ("empty list")
d [x] = 1
d xs  = prodDiff xs 1
    where
        prodDiff [x] res    = 1 * res
        prodDiff (x:xs) res = prodDiff xs (res * (x - (head xs)))
        
-- provided solution, again, think about the desired result
-- and write the function as if it already gives you want you want
-- i.e. want the difference between adjacent elements (x-y) times all the
--      other differences between adjacent elements: d (y:zs)
d' :: [Int] -> Int
d' [x]       =  1
d' (x:y:zs)  =  (x-y) * d (y:zs)
        
        
{-
    (c) Write a QuickCheck property prop_cd to confirm that c and d behave
        identically. Give the type signature of prop_cd and its definition.
        
        Note:  Forget how to exclude empty list from test data
               just test for 'null'
-}
prop_cd :: [Int] -> Bool
prop_cd lst = null lst || c lst == d lst



    
