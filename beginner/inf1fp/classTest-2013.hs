-- Informatics 1 - Functional Programming 
-- 2013 Class Test
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/exams/classexam-2013.pdf
--
-- Instructions state any function from the Standard Prelude plus the
-- following libraries may be used.
--
import Test.QuickCheck

import Data.Char
import Data.List

{-  1.
    (a) Write a function 
            f :: Char -> Int 
            
        that converts a character to its score. Lowercase letters score 5 
        if they are contained in the word "haskell"; otherwise they 
        score 1. Uppercase letters are worth double: 10 if contained in 
        "HASKELL", 2 otherwise. A character that is not a letter scores 
        zero. For example:
            f 'A' = 10 f 'B' = 2 f '.' = 0
            f 'a' = 5 f 'b' = 1
-}
f :: Char -> Int
f ch | not (isAlpha ch)                 = 0
     | isLower ch  && elem ch "haskell" = 5
     | isUpper ch  && elem ch "HASKELL" = 10
     | isLower ch                       = 1
     | otherwise                        = 2
  
{-
    (b) Using f, define a function 
            g :: String -> Int 
        that given a string returns the product of the score of every 
        letter in the string, ignoring any character that is not a letter. 
        For example:
            g "aBc4E" = 100 g "Inf1-FP" = 8 g "Java" = 50
        
        Your definition may use basic functions, list comprehension, and 
        library functions, but not recursion.
-}  
g :: String -> Int
g str = product [ f ch | ch <- str, isAlpha ch ]

{-
    (c) Again using f, define a function, 
            h :: String -> Int 
        that behaves identically to g, this time using basic functions 
        and recursion, but not list comprehension or library functions.
-}
h :: String -> Int
h (x:xs) | not (isAlpha x) = h xs
         | otherwise       = (f x) * (h xs)
h _ = 1
         
{- 2.
    (a) Write a function 
            c :: String -> String -> String 
        
        that takes two strings  and returns a string containing all
        matching characters (same character in the same position in both 
        strings). If one string is longer than the other, the extra
        characters should be ignored.
        
            c "parallel" "mutable" = "ale" 
            c "kangaroo" "potato" = ""
            c "flip" "flop" = "flp" 
            c "Flip" "flop" = "lp"
        
        Your definition may use basic functions, list comprehension, and 
        library functions, but not recursion.
-}         
c :: String -> String -> String
c xs ys = [ x | (x,y) <- zip xs ys, x == y ]

{-
    (b) Define a second function 
            d :: String -> String -> String 
            
        that behaves identically to c, this time using basic functions 
        and recursion, but not list comprehension or other library 
        functions.
-}
d :: String -> String -> String
d _ [] = []
d [] _ = []
d (x:xs) (y:ys) | x == y    = x : d xs ys
                | otherwise = d xs ys

{-
    (c) Write a QuickCheck property prop_cd to confirm that c and d
        behave identically. Give the type signature of prop_cd and its 
        definition.
-}                
prop_cd :: String -> String -> Bool
prop_cd xs ys = c xs ys == d xs ys

