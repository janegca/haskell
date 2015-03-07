{-# OPTIONS_GHC -Wall #-}
module Golf where

{-
    Week 03 - Recursion Patterns, Polymorphism, the Prelude
              Homework
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/hw/03-rec-poly.pdf
    
-}

import Data.List

{-
    Exercise 1 Hopscotch

    Your first task is to write a function
    
        skips :: [a] -> [[a]]

    The output of skips is a list of lists. The first list in the output 
    should be the same as the input list. The second list in the output 
    should contain every second element from the input list. . . and the 
    nth list in the output should contain every nth element from the 
    input list.

    For example:

    skips "ABCD" == ["ABCD", "BD", "C", "D"]
    skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
    skips [1] == [[1]]
    skips [True,False] == [[True,False], [False]]
    skips [] == []

    Note that the output should be the same length as the input.    

-}
skips :: [a] -> [[a]]
skips lst = process (length lst)
    where
        process 0 = []
        process n = process (n-1) 
                 ++ [[x | (x,y) <- zip lst [1..], mod y n == 0]] 
                                    
ex1a, ex1b, ex1c, ex1d, ex1e, ex1test :: Bool                  
ex1a = skips "ABCD" == ["ABCD", "BD", "C", "D"]
ex1b = skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
ex1c = skips [1 :: Int] == [[1 :: Int]]
ex1d = skips [True,False] == [[True,False], [False]]
ex1e = null $ skips []      -- check for empty list, []

ex1test = ex1a && ex1b && ex1c && ex1d && ex1e

{-
    Exercise 2 Local maxima

    A local maximum of a list is an element of the list which is strictly
    greater than both the elements immediately before and after it. For
    example, in the list [2,3,4,1,5], the only local maximum is 4, since
    it is greater than the elements immediately before and after it (3 and
    1). 5 is not a local maximum since there is no element that comes
    after it.
    
    Write a function
    
        localMaxima :: [Integer] -> [Integer]
    
    which finds all the local maxima in the input list and returns them in
    order. For example:
    
        localMaxima [2,9,5,6,1] == [9,6]
        localMaxima [2,3,4,1,5] == [4]
        localMaxima [1,2,3,4,5] == []

-}
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs) | x < y && z < y = y : localMaxima (y:z:zs)
                       | otherwise      = localMaxima (y:z:zs)
localMaxima  _ = []

ex2a, ex2b, ex2c, ex2test :: Bool
ex2a = localMaxima [2,9,5,6,1] == [9,6]
ex2b = localMaxima [2,3,4,1,5] == [4]
ex2c = localMaxima [1,2,3,4,5] == []
ex2test = ex2a && ex2b && ex2c

{-
    Exercise 3 Histogram

    For this task, write a function

        histogram :: [Integer] -> String

    which takes as input a list of Integers between 0 and 9 (inclusive),
    and outputs a vertical histogram showing how many of each number
    were in the input list. You may assume that the input list does not
    contain any numbers less than zero or greater than 9 (that is, it does
    not matter what your function does if the input does contain such
    numbers). Your output must exactly match the output shown in the
    examples below.  

        histogram [1,1,1,5] ==
            *
            *
            *    *
            ==========
            0123456789

        histogram [1,4,5,4,6,6,3,4,2,4,9] ==
                *
                *
                * *
             ******  *
            ==========
            0123456789

        Important note: If you type something like histogram [3,5] at
        the ghci prompt, you should see something like this:

        " * * \n==========\n0123456789\n"

        This is a textual representation of the String output, including \n
        escape sequences to indicate newline characters. 
        
        To actually visualize the histogram as in the examples above,
        use putStr, for example, putStr (histogram [3,5]).    

-}
histogram :: [Integer] -> String
histogram lst =  concat (hs (sort lst)) ++ "\n==========\n0123456789\n"
    where
        -- build output strings
        hs :: [Integer] -> [String]
        hs xs = map (pad 9) (rows (group xs)) 
        
        -- determine which elements appear in each output row
        rows :: [[Integer]] -> [[[Integer]]]
        rows xs  
            | and (map null xs) = []
            | otherwise         = rows (map (drop 1) xs) 
                                    ++ [(map (take 1) xs)]        
                  
        -- convert a row of elements into a formatted string          
        pad :: Integer -> [[Integer]] -> String
        pad n xss | n < 0       = "\n"
                  | elem n flat = pad (n-1) xss ++ "*"
                  | otherwise   = pad (n-1) xss ++ " "
            where flat = concat xss
            
      
ex4a, ex4b :: IO ()      
ex4a = putStr (histogram [1,1,1,5])
ex4b = putStr (histogram [1,4,5,4,6,6,3,4,2,4,9])
        