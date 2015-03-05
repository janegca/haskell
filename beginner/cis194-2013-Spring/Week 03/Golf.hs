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
        pairs = zip lst [1..]
        
        process 0 = []
        process n = process (n-1) 
                 ++ [[x | (x,y) <- pairs, mod y n == 0]] 
                  
--skips' :: [a] -> [[a]]
skips' lst = process (length lst)
    where
        pairs = zip lst [1..]
        
        process 0 = []
        process n = process (n-1)
                  ++ [map fst $ filter (\(x,y) -> mod y n == 0) pairs]
                  
                  
    

