-- Informatics 1 - Functional Programming 
-- Lecture 03 - Lists
--      
--
{-
    List Comprehensions
    
        based on mathematical Set notation
        all list members MUST be of the same type
        
        [ x*x | x <- [1,2,3] ]
            where, <- is called a 'generator'
                      and is pronounced as 'drawn from'
                      
        A list comprehension can be used anywhere a list is expected
        i.e. sum [1,2,3]
             sum [ x*x | x <- [1,2,3] ]
             product [ x*x | x <- [1,2,3] ]
                      
    Tuple
        unlike lists, can have values of different types
        often used for pairs, as below: (Int, String) 
        but can have multiple values all of same or different types
-}
import Data.Char (toLower, isAlpha)
import Test.QuickCheck

ex1 = [ x*x | x <- [1,2,3] ]
ex2 = [ toLower c | c <- "Hello, world" ]
ex3 = [ (x, even x) | x <- [1,2,3] ] -- result is a list of pairs (tuples)

{-
    Example Output:
    
        *Main> ex1
        [1,4,9]
        *Main> ex2
        "hello, world"
        *Main> ex3
        [(1,False),(2,True),(3,False)]
    
-}
{-
    List Comprehensions with Guards
    
        a 'guard' acts to filter what goes into the generated
        result i.e. in the examples below
            odd x       is a guard
            x > 0       is a guard
            isAlpha c   is a guard
            
        they MUST evaluate to True or False
        
        if the guard evaluates to False for the current 'x', that
        'x' will not be used to create the final result list
        
        can use multiple guards in the same list comprehension
        
        the 'comma' is pronounced 'such that'
        i.e. we could pronounce ex4 below as:
            'all x's where x is drawn from the list [1,2,3] such 
             that x is odd'
    
-}
                                        -- generate
ex4 = [ x | x <- [1,2,3], odd x ]       -- only odd numbers
ex5 = [ x*x | x <- [1,2,3], odd x ]     -- only square odd numbers
ex6 = [ x | x <- [42, -5, 24, 0, -3], 
            x > 0 ]                     -- only nums > 0
ex7 = [ toLower c | c <- "Hello, world!", 
                    isAlpha c ]         -- only chars in the alphabet
                    
ex7a = [ toLower c | c <- "Hello, world!",
                     isAlpha c && c < 'm' ]

{-
    Example Output:
    
        *Main> ex4
        [1,3]           -- only odd numbers from the list
        *Main> ex5
        [1,9]           -- only square of odd number from the list
        *Main> ex6
        [42,24]         -- only list elements > 0
        *Main> ex7
        "helloworld"    -- only string chars that are a to z
        *Main> ex7a     -- only alpha values less than 'm'
        "hellld"
-}

ex8 = sum []        -- returns additive identity 
ex9 = product []    -- returns multiplicative identity

{-
    Example Output:
    
        *Main> ex8
        0
        *Main> ex9
        1
    
-}
factorial :: Int -> Int
factorial n = product [1..n]

ex10 = factorial 4

-- EXAMPLES OF FUNCTIONS DEFINED USING LIST COMPREHENSIONS
-- the original list is passed to each function as an argument
-- 
squares :: [Integer] -> [Integer]
squares xs = [ x*x | x <- xs ]

odds :: [Integer] -> [Integer]
odds xs = [ x | x <- xs, odd x]

sumSqOdd :: [Integer] -> Integer
sumSqOdd xs = sum [ x*x | x <- xs, odd x ]

{-
    Example Usage:
    
        *Main> squares [1,2,3]
        [1,4,9]
        *Main> odds [1,2,3]
        [1,3]
        *Main> sumSqOdd [1,2,3]
        10
    
-}
{-
    Using QuickCheck to test functions
    - test function must begin with 'prop_'
    - must import QuickCheck (see to of this file)
    
    Example Usage:
        *Main> quickCheck prop_sumSqOdd
        +++ OK, passed 100 tests.
    
-}
prop_sumSqOdd :: [Integer] -> Bool
prop_sumSqOdd xs = sum (squares (odds xs)) == sumSqOdd xs


