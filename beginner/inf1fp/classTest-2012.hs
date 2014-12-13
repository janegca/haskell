-- Informatics 1 - Functional Programming 
-- 2012 Class Test
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/exams/classexam-2012.pdf
--
-- Instructions state any function from the Stanard Prelude plus the
-- following libraries may be used.
--
import Test.QuickCheck

import Data.Char
import Data.List
import Data.Maybe   -- this hasn't been covered in 2014 lectures 1-9

{-  1.
    (a) Write a function 
            f :: Char -> Bool 
        
        that determines whether or not an alphabetic character is in the 
        first half of the alphabet (letters before M, inclusive) or
        not. It should work for both upper and lower case letters. 
        For example,
            f 'e' == True f 'P' == False f 'q' == False
            f 'G' == True f 'n' == False f 'M' == True

        For any character that is not an alphabetic character, f should
        return an error.
-}
f :: Char -> Bool
f ch = ord (toLower ch) <= ord (toLower 'm')

{-
    (b) Using f, define a function 
            g :: String -> Bool 
        
        that given a string returns True if the string contains more 
        letters in the first half of the alphabet than in
        the second half, ignoring any character that is not an alphabetic 
        character. For example,
            g "SyzYGy" == False g "aB7L!e" == True g "" == False
            g "Aardvark" == True g "emnity" == False
        
        Your definition may use basic functions, list comprehension, and 
        library functions, but not recursion.
-}
g :: String -> Bool
g xs = sum [ 1 | x <- xs, f x] > sum [ 1 | x <- xs, not (f x) ]

{-
    (c) Again using f, define a function 
            h :: String -> Bool 
        
        that behaves identically to g, this time using basic functions 
        and recursion, but not list comprehension or library functions.
-}
h :: String -> Bool
h xs = (check 0 xs) > 0
    where 
        check res (x:xs) | f x       = check (res + 1) xs
                         | otherwise = check (res - 1) xs
        check res _ = res
         
{-
    2.
    (a) Write a function 
            c :: [Int] -> [Int] 
        that returns a list containing all of the elements in the argument
        list that occur twice in succession. If an element occurs n times 
        in succession, for n >= 2, then it should it appear n-1 times in 
        succession in the result. The value of the function applied to the
        empty list need not be defined.
        
            c [3,1,1,3,3,5] == [1,3]   c [2,1,4,1,2] == []
            c [4,1,1,1,4,4] == [1,1,4] c [3,3,1,3,1] == [3]
            c [2,2,2,2,2] == [2,2,2,2] c [42] == []
        
        Your definition may use basic functions, list comprehension, and 
        library functions, but not recursion.
-}         
c :: [Int] -> [Int]
c xs = [ x | (x,y) <- zip xs (tail xs), x == y ]

{-
    (b) Define a second function 
            d :: [Int] -> [Int] 
        
        that behaves identically to c, this time using basic functions
        and recursion, but not list comprehension or other library 
        functions.
-}
d :: [Int] -> [Int]
d (x:y:zs) | x == y     = x : d (y:zs)
           | otherwise  = d (y:zs)
d _ = []         

{-
    (c) Write a QuickCheck property prop_cd to confirm that c and d 
        behave identically. Give the type signature of prop_cd and its
        definition.

-}
prop_cd :: [Int] -> Bool
prop_cd xs = c xs == d xs
