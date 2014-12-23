-- Informatics 1 - Functional Programming 
-- Tutorial Review - 6 - List Comprehensions, Recursion, High Order Fns
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/revision/tutorial6.pdf
--
import Test.QuickCheck

{- 1.
    (a) Write a polymorphic function 
            f1 :: [a] -> [a] 
        
        that returns every third element in a list, starting with the 
        first. For example:
        
            f1 "abcdefghij" == "adgj"
            f1 [1,2,3,4,5] == [1,4]
            f1 [0,0,0,0,0] == [0,0]
            f1 [] == []
            
        Your function may use basic functions, list comprehension, and 
        library functions, but not recursion.
        
    (b) Write a second function 
                g1 :: [a] -> [a] 
        
        that behaves like f1, this time using basic functions and 
        recursion, but not list comprehension or library functions.
        
    (c) Write a third function 
            h1 :: [a] -> [a] 
        
        that also behaves like f1, this time using one or more of the
        following higher-order library functions:
        
            map :: (a -> b) -> [a] -> [b]
            filter :: (a -> Bool) -> [a] -> [a]
            foldr :: (a -> b -> b) -> b -> [a] -> b

        You may also use basic functions, but not list comprehension, 
        other library functions, or recursion.
-}
f1 :: [a] -> [a]
f1 xs = [ xs !! i | i <- [0,3..length xs - 1] ]

g1 :: [a] -> [a]
g1 []         = []
g1 (x:y:z:xs) = x : g1 xs
g1 xs         = [head xs]

h1 :: [a] -> [a]
-- provided solution
h1 xs = map (\i -> xs !! i) [0,3..length xs - 1]

prop_q1 :: Eq a => [a] -> Bool
prop_q1 xs = f1 xs == g1 xs && g1 xs == h1 xs

{- 2.
    (a) Write a polymorphic function 
            f2 :: [a] -> [a] 
            
        that swaps every two items in a list. Your function should swap 
        the first with the second item, the third with the fourth, and so 
        on. You may assume that the length of an input list is even.
        For example:
        
                f2 "swapping" == "wspaipgn"
                f2 [1,2,3,4] == [2,1,4,3]
                f2 [] == []

        Your function may use basic functions, list comprehension, and 
        library functions, but not recursion.
        
    (b) Write a second function 
            g2 :: [a] -> [a] 
        
        that behaves like f2, this time using basic functions and 
        recursion, but not list comprehension or other library functions.
        
    (c) Write a third function 
                h2 :: [a] -> [a] 
        
        that also behaves like f2, this time using one or more of the 
        following higher-order library functions:
        
            map :: (a -> b) -> [a] -> [b]
            filter :: (a -> Bool) -> [a] -> [a]
            foldr :: (a -> b -> b) -> b -> [a] -> b
            
        You may also use basic functions, but not list comprehension, 
        other library functions, or recursion.

-}
f2 :: [a] -> [a]
-- provided solution
f2 xs = concat [[xs !! (i+1), xs !! i]| i <- [0,2..length xs - 1]]

g2 :: [a] -> [a]
g2 (x:y:xs) = y : x : g2 xs
g2 _ = []

h2 :: [a] -> [a]
h2 xs = concat (map (\i -> [xs !! (i+1), xs !! i]) [0,2..length xs - 1])

-- provided solution
prop_q2 :: Eq a => [a] -> Property
prop_q2 xs = (even (length xs)) ==> f2 xs == g2 xs && g2 xs == h2 xs

{- 3.
    (a) Write a function 
            f3 :: [Int] -> Int 
        
        that computes the sum of the products of adjacent elements in a
        list of even length, as shown below. The function should give an 
        error if provided a list of odd length. For example:
        
                f3 [1,2,3,4] = 1*2 + 3*4 = 14
                f3 [3,5,7,5,-2,4] = 3*5 + 7*5 + (-2)*4 = 42
                f3 [] = 0
                f3 [1,2,3] = error

        Use basic functions, list comprehension, and library functions, 
        but not recursion.
    
    (b) write a recursive version
    
    (c) write a version using any or all of map, filter, foldr 

-}
f3 :: [Int] -> Int
f3 xs = sum [xs !! (i+1) * xs !! i | i <- [0,2..length xs - 1]]

g3 :: [Int] -> Int
g3 []       = 0
g3 (x:y:xs) = x * y + g3 xs
g3 _        = error ("list not even")

h3 :: [Int] -> Int
h3 xs = sum (map (\i -> xs !! (i+1) * xs !! i) [0,2..length xs - 1])

prop_q3 :: [Int] -> Property
prop_q3 xs = even(length xs) ==> f3 xs == g3 xs && g3 xs == h3 xs


