-- Informatics 1 - Functional Programming 
-- Lecture 4 - Lists and Recursions

{-
    Function to return odd numbers written as a list comprehension,
    recursively and in imperative (conditional) style
    
    Pattern matching can include guards, Haskell will pick the case
    that first matches the pattern (i.e. [] or (x:xs)), and then
    on the case which passes the guard; if the first guard returns
    False, the next expression is checked; and so on until 
    'otherwise' which acts as a default process
    
    Walkthrough of 
        oddsRec [1,2,3]
        ==> oddsRec (1 : (2 : (3 : [])))
        ==> 1 : oddsRec(2 : (3 : []))    -- x=1, (odd 1) = True, xs=[2,3]
        ==> 1 : oddsRec(3 : []))         -- x=2, (odd 2) = False, xs=[3]
        ==> 1 : (3 : (oddsRec []))       -- x=3, (odd 3) = True, xs = []
        ==> 1 : (3 : []))
        ==> [1,3]
   
-}
import Test.QuickCheck

-- written using list comprehension
odds :: [Integer] -> [Integer]
odds xs = [ x | x <- xs, odd x ]

-- written as a recursive definition
oddsRec :: [Integer] -> [Integer]
oddsRec []     = []
oddsRec (x:xs) | odd x     = x : oddsRec xs    -- guard
               | otherwise = oddsRec xs
            
-- written in imperative style with recursion
oddsCond :: [Integer] -> [Integer]
oddsCond ws =
    if null ws then
        []
    else
        let
            x  = head ws
            xs = tail ws
        in
            if odd x then
                x : oddsCond xs
            else
                oddsCond xs            
            
prop_odds :: [Integer] -> Bool
prop_odds xs = (odds xs) == (oddsRec xs)
             && (oddsRec xs) == (oddsCond xs)
             
{-
    Test results:
    
        *Main> quickCheck prop_odds
        +++ OK, passed 100 tests.
    
-}             
{-
    Accumulation - reducing a list to a single value
        can't be written using List Comprehension as it doesn't
        return a list
        
    Walkthrough of 
        sumRec [1,2,3]
        ==> sumRec (1 : (2 : (3 : [])))
        ==> 1 + sumRec (2 : (3 : []))
        ==> 1 + (2 + sumRec (3 : []))
        ==> 1 + (2 + (3 + (sumRec []))
        ==> 1 + (2 + (3 + 0))
        ==> 6
   
-}
sumRec :: [Integer] -> Integer
sumRec []     = 0
sumRec (x:xs) = x + sumRec xs

prop_sum :: [Integer] -> Bool
prop_sum xs = sum xs == sumRec xs       -- sum is built-into Prelude

{-
    *Main> quickCheck prop_sum
    +++ OK, passed 100 tests.
-}
productRec :: [Integer] -> Integer
productRec []     = 1
productRec (x:xs) = x * productRec xs

prop_product :: [Integer] -> Bool
prop_product xs = product xs == productRec xs

{-
    *Main> quickCheck prop_product
    +++ OK, passed 100 tests.
-}

{-
    Putting functions together
        Take the sum of the squares of odd numbers
        Two definitions, one using list comprehension, the other
        using recursion
-}
sumSqOdd :: [Integer] -> Integer
sumSqOdd xs = sum [ x*x | x <- xs, odd x ]

sumSqOddRec :: [Integer] -> Integer
sumSqOddRec []     = 0
sumSqOddRec (x:xs) | odd x     = x*x + sumSqOddRec xs
                   | otherwise = sumSqOddRec xs
                   
prop_sumOfOdds :: [Integer] -> Bool
prop_sumOfOdds  xs = sumSqOdd xs == sumSqOddRec xs                

{-
    *Main> quickCheck prop_sumOfOdds
    +++ OK, passed 100 tests.
-}  
             
