-- Informatics 1 - Functional Programming 
-- 2013 Class Test
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/exams/classexam-2014.pdf
--
-- Instructions state any function from the Standard Prelude plus the
-- following libraries may be used.
--
import Test.QuickCheck

import Data.Char
import Data.List

{-  1.
    (a) In typography, a descender is the portion of a character that 
        extends below the line, for example the "tail" in the letter y. 
        Write a function 
            f :: Char -> Bool
            
        that returns True for letters with descenders and False otherwise.
        For example:
                f 'a' == False f 'p' == True f 'A' == False
                f 'P' == False f '3' == False

        Which letters have descenders is dependent on font, but for this 
        question assume that no upper-case letters have descenders and 
        that only the following lower-case letters have descenders: 
        g, j, p, q, y.

-}
f :: Char -> Bool
f ch = elem ch ['g','j','p','q','y']

{-
    (b) Using f, define a function 
            g :: String -> Int 
        that given a string returns the number of letters in the string 
        that have descenders. For example:
        
            g "prig" == 2 g "minimum" == 0 g "" == 0
            g "42NATly" == 1 g "Jiggle" == 2

        Your definition may use basic functions, list comprehension, and 
        library functions, but not recursion.
-}
g :: String -> Int
g str = sum [ 1 | x <- str, f x]

{-
    (c) Again using f, define a function, 
            h :: String -> Int 
        
        that behaves identically to g, this time using basic functions 
        and recursion, but not list comprehension or library functions.
-}
h :: String -> Int
h str = sum (count str)
    where
        count (x:xs) | f x       = 1 : count xs
                     | otherwise = count xs
        count _ = []

{-
    2.
    (a) Write a function 
            c :: String -> String 
            
        that converts all characters in positions 0; 2; 4; : : : to upper
        case, numbering from 0. For example:
            c "haskell" == "HaSkElL"     c "" == ""
            c "Edinburgh" == "EdInBuRgH" c "83wing" == "83WiNg"
        
        Your definition may use basic functions, list comprehension, and 
        library functions, but not recursion.
-}
c :: String -> String
c str = [  cap (x,y) | (x,y) <- zip str [0..] ]
    where
        cap (ltr, pos) | even pos    = toUpper ltr
                       | otherwise   = ltr
                                              
{-
    (b) Define a second function 
            d :: String -> String 
        that behaves identically to c, this time using basic functions 
        and recursion, but not list comprehension or other library
        functions.
-}        
d :: String -> String
d []         = []
d (x:[])     = [toUpper x]
d (x:y:xs)   = toUpper x : y : d xs

{-  
    (c) Write a QuickCheck property prop_cd to confirm that c and d behave
        identically. Give the type signature of prop_cd and its definition.
-}
prop_cd :: String -> Bool
prop_cd str = c str == d str

