-- sample solutions for Inf1-FP Revision Tutorial 4

import Data.List
import Data.Char
import Test.QuickCheck

-- baseName is recursive, baseName' is list comprehension,
-- baseName'' is higher order version, prop_baseName is the
-- test.

doubleOdds :: [Int] -> [Int]
doubleOdds [] = []
doubleOdds (x:xs) | odd x     = 2 * x : doubleOdds xs
                  | otherwise = doubleOdds xs
                  
doubleOdds' xs = [2 * x | x <- xs, odd x]

doubleOdds'' = map (2*) . filter odd

prop_doubleOdds :: [Int] -> Bool
prop_doubleOdds xs = doubleOdds xs == doubleOdds' xs && doubleOdds' xs == doubleOdds'' xs

numeric :: String -> String
numeric "" = ""
numeric (x:xs) | isDigit x = x : numeric xs
               | otherwise = numeric xs
               
numeric' xs = [x | x <- xs, isDigit x]

numeric'' = filter isDigit

prop_numeric :: String -> Bool
prop_numeric str = numeric str == numeric' str && numeric' str == numeric'' str

addPairwise :: [Int] -> [Int] -> [Int]
addPairwise [] _ = []
addPairwise _ [] = []
addPairwise (x:xs) (y:ys) = x + y : addPairwise xs ys

addPairwise' xs ys = [x + y | (x,y) <- zip xs ys]

addPairwise'' = zipWith (+) 

prop_addPairwise :: [Int] -> [Int] -> Bool
prop_addPairwise xs ys = addPairwise xs ys == addPairwise' xs ys && addPairwise xs ys == addPairwise'' xs ys

addUp :: [[Int]] -> [Int]
addUp [x] = x
addUp (x:xs) = addPairwise x (addUp xs)

addUp' :: [[Int]] -> [Int]
addUp' []    = [] -- minimum does not handle []
addUp' lists = [sum [list !! pos | list <- lists] | pos <- [0 .. minLen-1]]
              where
                  minLen = minimum [length l | l <- lists]

addUp'' = foldr1 addPairwise

prop_addUp :: [[Int]] -> Property
prop_addUp xs = not (null xs) ==> addUp xs == addUp' xs && addUp' xs == addUp'' xs
