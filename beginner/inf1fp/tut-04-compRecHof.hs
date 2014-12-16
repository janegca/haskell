-- Informatics 1 - Functional Programming 
-- Tutorial 4 - Revies of List Comprehensions, Recursion, High Order Functions
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/revision/tutorial4.pdf
--
{-
    General Instructions
    --------------------
    
    In this tutorial you will be asked to code simple functions. Always 
    give a recursive definition and a list comprehension version. 
    For an additional challenge you can try a version using higher order 
    functions like map, filter and foldr. To check your solutions write a 
    QuickCheck test to test the correctness of your solutions.
-}
import Test.QuickCheck
import Data.Char (isDigit)

{- 1. Write a function 
        doubleOdds :: [Int] -> [Int] 
      that will double every odd number in the list:
      
        doubleOdds [0,2,1,7,8,56,17,18] == [2, 14, 34]
-}
doubleOdds :: [Int] -> [Int]
doubleOdds xs = [ x * 2 | x <- xs, odd x ]

doubleOddsRec :: [Int] -> [Int]
doubleOddsRec [] = []
doubleOddsRec (x:xs) | odd x = x * 2 : doubleOddsRec xs
                     | otherwise = doubleOddsRec xs
  
doubleOddsHof :: [Int] -> [Int]
doubleOddsHof = map (*2) . filter odd

prop_doubleOdds :: [Int] -> Bool
prop_doubleOdds xs = doubleOdds xs == doubleOddsRec xs
                  && doubleOdds xs == doubleOddsHof xs
    
{-
    2. Write a function     
            numeric :: String -> String 
            
       that will leave a string only with numbers in it:
       
            numeric "1 and 1 are 2" == "112"
-}    
numeric :: String -> String
numeric xs = [ x | x <- xs, isDigit x ]                  

numericRec :: String -> String
numericRec (x:xs) | isDigit x = x : numericRec xs
                  | otherwise = numericRec xs
numericRec _ = []

numericHof = filter isDigit

prop_numeric :: String -> Bool
prop_numeric str = numeric str == numericRec str
                && numeric str == numericHof str

{-
    3. Write a function 
            addPairwise :: [Int] -> [Int] -> [Int] 
       
       that does element-wise addition on two lists:
       
            addPairwise [3, 4, 5, 6] [2, 4, 6] 
            == [3 + 2, 4 + 4, 5 + 6] == [5, 8, 11]
-}                
addPairwise :: [Int] -> [Int] -> [Int]
addPairwise xs ys = [ x + y | (x,y) <- zip xs ys ]

addPairwiseRec :: [Int] -> [Int] -> [Int]
addPairwiseRec (x:xs) (y:ys) = x + y : addPairwise xs ys
addPairwiseRec _ _ = []

addPairwiseHof :: [Int] -> [Int] -> [Int]
addPairwiseHof = zipWith (+)

prop_addPairwise :: [Int] -> [Int] -> Bool
prop_addPairwise xs ys = addPairwise xs ys == addPairwiseRec xs ys
                      && addPairwise xs ys == addPairwiseHof xs ys
                      
{-
    4. Using addPairwise write a function 
            addUp :: [[Int]] -> [Int] 
       that does the same for any number of lists:
            addUp [[3, 4, 5, 6], [2, 4, 6], [1, 5]] 
            == [3 + 2 + 1, 4 + 4 + 5] == [6, 13]
-}           
-- from provided solution
addUp :: [[Int]] -> [Int]
addUp []    = []       -- minimum does not handle []
addUp lists = [sum [list !! pos | list <- lists] | pos <- [0 .. minLen-1]]
              where
                  minLen = minimum [length l | l <- lists]    

addUpRec :: [[Int]] -> [Int]
addUpRec [xs]      = xs
addUpRec (xs:xss)  = addPairwise xs (addUpRec xss)
addUpRec _ = []

-- from provided solution
addUpHof :: [[Int]] -> [Int]
-- foldr1 has no starting value and must be applied to non empty lists
addUpHof = foldr1 addPairwise


                     
