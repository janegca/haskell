-- Informatics 1 - Functional Programming 
-- Lecture 6 - Even More Recursion
--
import Test.QuickCheck

{-
    Counting
        [1..3] is syntactic sugar for enumFromTo 1 3
        
        [1,3,..20] will create a list counting by 2's
                   based on the pattern between 1 and 3
                   
        [1..] will create an infinite list, ie it won't stop
              until it is interrupted
              this is syntactic sugar for enumFrom m n
              
        will work with chars as internally they are converted
        to ASCII ordinal numbers
        
        ['a'..'z'] gives alphabet in lowercase
        ['A'..'Z'] gives alphabet in uppercase
        
-}
ex1  = [1..3]
ex2  = enumFromTo 1 3
ex3  = [1,3..20]        -- odd numbers 1 to 20
ex3a = [2,4..20]        -- even numbers 1 to 20
ex4  = ['a'..'z']
ex5  = ['A'..'Z']


{-
    *Main> ex1
    [1,2,3]
    *Main> ex2
    [1,2,3]
    *Main> ex3
    [1,3,5,7,9,11,13,15,17,19]
    *Main> ex3a
    [2,4,6,8,10,12,14,16,18,20]    
    *Main> ex4
    "abcdefghijklmnopqrstuvwxyz"
    *Main> ex5
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    
-}
{-
    Recursive definition of enumFromTo
    
        enumFromTo :: Int -> Int -> [Int]
        enumFromTo m n | m > n = []
                       | m <= n = m : enumFromTo (m+1) n    
                       
    Walk through:
        enumFromTo 1 3
        ==> 1 : enumFromTo 2 3
        ==> 1 : (2 : enumFromTo 3 3)
        ==> 1 : (2 : (3 : enumFromTo 4 3))
        ==> 1 : (2 : (3 : []))
        ==> [1,2,3]
-}
{-
    Factorial
        Can be defined using the library function 'product'
        and recursively with the use of a HELPER function
        
        Helper functions are 'local' which means they are visible
        only within the defining function. They are usually to
        written to handle a specialized behaviour that is unlikely
        to be needed by other functions.
        
-}
factorial :: Int -> Int
factorial n = product [1..n]

factRec :: Int -> Int
factRec n = fact 1 n
    where  
        -- helper function 'fact' is recursive and defined within 
        -- (visible only to) the factRec function
        fact :: Int -> Int -> Int
        fact m n | m > n = 1
                 | m <= n = m * fact (m+1) n
                 
{-
    *Main> factorial 10
    3628800
    *Main> factRec 10
    3628800
-}
{-
    Zip Function
        combines elements of two lists into a list of pairs
 
    Defined as:
    
        zip :: [a] -> [b] -> [(a,b)]
        zip [] ys = []
        zip xs [] = []
        zip (x:xs) (y:ys) = (x,y) : zip xs ys    

    The length of the resulting list will be equal to the
    shortest list
    
-}
ex6 = zip [0,1,2] "abc"
ex7 = zip [] "abc"
ex8 = zip [0,1,2] []
ex9 = zip [1..5] ['a'..'z']
ex10 xs = zip xs (tail xs)      -- list of consecutive chars

{-
    *Main> ex6
    [(0,'a'),(1,'b'),(2,'c')]
    *Main> ex7
    []
    *Main> ex8
    []
    *Main> ex9
    [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')] 
    *Main> ex10 "words"
    [('w','o'),('o','r'),('r','d'),('d','s')]

-}
{-
    Alternative definition
        Forces both lists to be of the same length        
-}
zipHarsh :: [a] -> [b] -> [(a,b)]
zipHarsh [] [] = []
zipHarsh [] ys = error ("Strings must be same length")
zipHarsh xs [] = error ("Strings must be same length")
zipHarsh (x:xs) (y:ys) = (x,y) : zipHarsh xs ys
  
ex11 = zip [0,1,2] "abcde"  
ex12 = zipHarsh [0,1,2] "abcde"     -- produces an error

{-
    *Main> ex12
    *** Exception: Strings must be same length
-}
{-
    Example for taking the 'dot' product of two lists (using them
    as vectors).
-}
dot :: Num a => [a] -> [a] -> a
dot xs ys = sum [ x*y | (x,y) <- zipHarsh xs ys ]

dotRec :: Num a => [a] -> [a] -> a
dotRec [] [] = 0
dotRec (x:xs) (y:ys) = x*y + dotRec xs ys

{-
    *Main> dot [2,3,4] [5,6,7] == dotRec [2,3,4] [5,6,7]
    True
-}
{-
    Search example
        Returns the index positions of char if it's found in the
        string
    
        Eq a => is a constraint, any type passed in must also belong to
                the Eq (equal) type class i.e. the values of type 'a'
                must be comparable
-}
search :: Eq a => [a] -> a -> [Int]
search xs y = [ i | (i,x) <- zip [0..] xs, x==y ]

searchRec :: Eq a => [a] -> a -> [Int]
searchRec xs y = srch xs y 0
    where
        srch :: Eq a => [a] -> a -> Int -> [Int]
        srch [] y i = []
        srch (x:xs) y i
            | x == y = i : srch xs y (i+1)
            | otherwise = srch xs y (i+1)
        
ex13 = search "bookshop" 'o'

{-
    *Main> ex13
    [1,2,6]
-}
{-
    Select, take and drop as list comprehensions
    
        "words" !! 3    -> 'd'
        take 3 "words"  -> "wor"
        drop 3 "words"  -> "ds"
-}
selectComp :: [a] -> Int -> a -- (!!)
selectComp xs i = the [ x | (j,x) <- zip [0..] xs, j == i ]
    where
    the [x] = x

-- this will not stop after taking 3, no 'stop' point, will
-- just continue checking position values j<i
takeComp :: Int -> [a] -> [a]
takeComp i xs = [ x | (j,x) <- zip [0..] xs, j < i ]

-- this too will go on forever with an infinite list
dropComp :: Int -> [a] -> [a]
dropComp i xs = [ x | (j,x) <- zip [0..] xs, j >= i ]

{-
    Definitions of select, take and drop using recursion 
    (simplification of definitions in the Prelude)

        (!!) :: [a] -> Int -> a
        (x:xs) !! 0 = x
        (x:xs) !! i = xs !! (i-1)

        take :: Int -> [a] -> [a]
        take 0 xs = []
        take i [] = []
        take i (x:xs) = x : take (i-1) xs

        drop :: Int -> [a] -> [a]
        drop 0 xs = xs
        drop i [] = []
        drop i (x:xs) = drop (i-1) xs

-}

