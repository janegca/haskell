-- List operations
module ListOps where

import Data.Maybe

{-
    References:
        [DM]     Discrete Math using a Computer
        [INFP]  Informatics Functional Programming Course
-}

-- count the number of times an element appears in a list [DM]
count :: Eq a => a -> [a] -> Int
count n [] = 0
count n (x:xs) | n == x    = 1 + count n xs
               | otherwise = count n xs

-- delete an element from a list [DM]
del :: Eq a => a -> [a] -> [a]
del e (x:xs) | e == x    = del e xs
             | otherwise = x : del e xs
del _ _ = []

-- take the dot product of two lists [INFP]
dot :: Num a => [a] -> [a] -> a
dot xs ys = sum [ x*y | (x,y) <- zipHarsh xs ys ]
      
-- extract values from a list of Maybe values [DM]
extract :: [Maybe a] -> [a]
extract []             = []
extract (Nothing : xs) = extract xs
extract (Just x : xs)  = x : extract xs

-- partition a list on the results of a given function [HR]
-- Ex fct2listpart even [1..5] -> [[1,3,5],[2,4]]
fct2listpart :: (Eq a, Eq b) => (a -> b) -> [a] -> [[a]]
fct2listpart f []     = []
fct2listpart f (x:xs) = xclass : fct2listpart f (xs \\ xclass)
    where xclass = x : [ y | y <- xs, f x == f y ]

-- return the intersection of two lists (items appearing in both) [DM]
intersection :: Eq a => [a] -> [a] -> [a]
intersection xs [] = []
intersection [] ys = []
intersection (x:xs) (y:ys)
    | (elem x ys) && (elem y xs) = x : y : intersection xs ys
    | elem x ys                  = x : intersection xs ys
    | elem y xs                  = y : intersection xs ys
    | otherwise                  = intersection xs ys

-- returns True if all elements in the first list are in the second [DM]    
isSubset :: Eq a => [a] -> [a] -> Bool
isSubset xs []     = False
isSubset [] ys     = True
isSubset (x:xs) ys = elem x ys && isSubset xs ys       
    
-- given a list of pairs, return the second element given the first [DM]
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' n ((x,y):xs) | n == x     = Just y
                    | otherwise  = lookup' n xs
lookup' _ _ = Nothing

-- a function to generate sublists of a list [HR]
-- Ex: powerList [1,2,3] -> [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
powerList :: [a] -> [[a]]
powerList []     = [[]]
powerList (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))

-- give all the ways to split a list into non-empty parts [HR]
-- Ex: split [1,2,3,4] -> [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
split :: [a] -> [([a],[a])]
split [x,y]    = [([x],[y])]
split (x:zs) =
    ([x],zs) : (map (\ (us,vs) -> ((x:us),vs)) (split zs))


-- zip but only if both lists are the same size [INFP]   
zipHarsh :: [a] -> [b] -> [(a,b)]
zipHarsh [] [] = []
zipHarsh [] ys = error ("Strings must be same length")
zipHarsh xs [] = error ("Strings must be same length")
zipHarsh (x:xs) (y:ys) = (x,y) : zipHarsh xs ys
