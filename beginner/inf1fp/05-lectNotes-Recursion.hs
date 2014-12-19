-- Informatics 1 - Functional Programming 
-- Lecture 5 - More Recursion

{-
    Conditionals
    
    Conditionals are your enemy: each conditional doubles the number of 
    test cases you must consider. A program with five two-way conditionals 
    requires 25 = 32 test cases to try every path through the program. 
    A program with ten two-way conditionals requires 210 = 1024 test cases.

    Clarity is more important than efficiency.

-}
import Test.QuickCheck

max' :: Int -> Int -> Int
max' x y | x >= y = x
         | otherwise = y

max3 :: Int -> Int -> Int -> Int
max3 x y z | x >= y && x >= z = x
           | y >= x && y >= z = y
           | otherwise        = z
           
max3' :: Int -> Int -> Int -> Int
max3' x y z = if x >= y then
                if x >= z then x else z
              else
                if y >= z then y else z           
                
-- best way to define max3
max3'' :: Int -> Int -> Int -> Int
max3'' x y z = x `max'` y `max'` z                

{-
    Associativity
        Is doesn't matter where parentheses go with an associative
        operator
     
-}
prop_max_assoc :: Int -> Int -> Int -> Bool
prop_max_assoc x y z =
    (x `max` y) `max` z == x `max` (y `max` z)

{-
    Append (++) 
    
        (++) :: [a] -> [a] -> [a]
        [] ++ ys = ys
        (x:xs) ++ ys = x : (xs ++ ys)
        
    Walk through 
        "abc" ++ "de"
        ==> ('a' : ('b' : ('c' []))) ++ "de"
        ==> 'a' : ('b' : ('c' [])) ++ "de"
        ==> 'a' : ('b' : ('c' [])) ++ "de"
        ==> 'a' : ('b' : ('c' :([] ++ "de")))
        ==> "abcde"
        
        4 steps, so xs ++ ys takes length(xs)+1 steps

-}    
{-
    Operators that are left associative, group from the left:
        ((xs1 ++ xs2) ++ xs3 ) ++ xs4
        
    Operators that are right associative group from the right:
        xs1 ++ (xs2 ++ (xs3 ++ xs4))
        
    Where n1, n2, n3,... are the lengths of xs1, xs2, xs3, ...
    Associating from the left takes
        n1 + (n1 + n2) + (n1 + n2 + n3) 
    steps so if we have m lists of length n then it takes (m^2)n
    steps.
    
    Associating from the right takes
        n1 + n2 + n3
    steps so if we have m lists of length n it takes mn steps
    If m = 1000, right associativity is roughly 1000 times faster
    than left associativity
    
-}    
