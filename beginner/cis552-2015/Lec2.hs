{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fwarn-tabs -fno-warn-type-defaults #-}
module Lec2 where
{-
    CIS 552: Advanced Programming (2015)
    Lecture 2 - Lists and Recursion
    
    Source:
    https://www.seas.upenn.edu/~cis552/lectures/stub/Lec2.html

-}

import Data.Char
import Test.HUnit

import Prelude

-- list are sequences of values of the same type
l1 :: [Char]
l1 = ['a','b','c']

l2 :: [Int]
l2 = [1,2,3]

-- lists can contain structured data
l3 :: [(Int,Bool)]
l3 = [(1,True),(2,False)]

-- lists can be nested
l4 :: [[Int]]
l4 = [[1,2,3],[4,5]]

-- lists can be empty
l6 :: [a]
l6 = []

-- String is another name for a list of characters
l7 :: String
l7 = ['h','e','l','l','o',' ','5','5','2','!']

--Constructing lists
cons :: a -> [a] -> [a]
cons = (:)

c1 :: [Char]
c1 = 'a' : ['b','c']

c2 :: [Int]
c2 = 1 : []

--c3 :: [a]
--c3 = [] : []      -- fails

{- ---------------------------------------------------------------
    Practise 1:
        Write a function that, given an argument, x, and a number,
        n, returns a list containing n copies of x.
        
        Step 1. Define test cases for the function
        Step 2. Define the type of the function
        Step 3. Implement the function
        Step 4. Run the tests
        Step 5. Refine the definition to make it more readable
  ---------------------------------------------------------------- -}
--  Step 1
testClone1, testClone2, testClone3 :: Test
testClone1 = clone 'a' 4 ~?= ['a','a','a','a']
testClone2 = clone 'a' 0 ~?= []
testClone3 = clone 1.1 3 ~?= [1.1,1.1,1.1]

-- Step 2 & 3
clone :: a -> Int -> [a]
clone _ 0 = []
clone x n =  x : clone x (n-1)

-- Step 4
cloneTests :: IO Counts
cloneTests = runTestTT (TestList [ testClone1, testClone2, testClone3 ])

{-
    *Lec2> p1Tests

    Cases: 3  Tried: 0  Errors: 0  Failures: 0
    Cases: 3  Tried: 1  Errors: 0  Failures: 0
    Cases: 3  Tried: 2  Errors: 0  Failures: 0
                                              
    Cases: 3  Tried: 3  Errors: 0  Failures: 0
    Counts {cases = 3, tried = 3, errors = 0, failures = 0}
    *Lec2> 

-}

-- Step 5
clone' :: a -> Int -> [a]
clone' x n = take n (repeat x)

cloneTests' :: IO Counts
cloneTests' = runTestTT (TestList [ 
          clone' 'a' 4 ~?= ['a','a','a','a'],
          clone' 'a' 0 ~?= [],
          clone' 1.1 3 ~?= [1.1, 1.1, 1.1]
         ])

{-
    *Lec2> cloneTests'

    Cases: 3  Tried: 0  Errors: 0  Failures: 0
    Cases: 3  Tried: 1  Errors: 0  Failures: 0
    Cases: 3  Tried: 2  Errors: 0  Failures: 0
                                              
    Cases: 3  Tried: 3  Errors: 0  Failures: 0
    Counts {cases = 3, tried = 3, errors = 0, failures = 0}
    *Lec2> 

-}     
{- ------------------------------------------------------------------
    Practise 2
        Define a function that, given two integers i and j, returns
        a list of all the numbers at least as big as i but no greater
        than j, in order
        
        Step 1. Define test cases
        Step 2. Declare the type
        Step 3. Define the function
        Step 4. Run tests
        Step 5. Refactor
-}    
-- Step 1
testRange :: Test
testRange = TestList [ range 3  6  ~?= [3,4,5,6],
                       range 42 42 ~?= [42],
                       range 10 5  ~?= [] ]
                       
-- Step 2 & 3                       
range :: Int -> Int -> [Int]
range i j = if i > j then [] else i : range (i+1) j

-- Step 4
runRTests :: IO Counts
runRTests = runTestTT testRange

{-
    *Lec2> runRTests

    Cases: 3  Tried: 0  Errors: 0  Failures: 0
    Cases: 3  Tried: 1  Errors: 0  Failures: 0
    Cases: 3  Tried: 2  Errors: 0  Failures: 0
                                              
    Cases: 3  Tried: 3  Errors: 0  Failures: 0
    Counts {cases = 3, tried = 3, errors = 0, failures = 0}

-}

-- Step 5
range' :: Int -> Int -> [Int]
range' i j = [i..j]                       
    
runRTests' :: IO Counts
runRTests' = runTestTT (TestList [ 
                            range' 3  6  ~?= [3,4,5,6],
                            range' 42 42 ~?= [42],
                            range' 10 5  ~?= [] ] )
                            
{-
    *Lec2> runRTests'

    Cases: 3  Tried: 0  Errors: 0  Failures: 0
    Cases: 3  Tried: 1  Errors: 0  Failures: 0
    Cases: 3  Tried: 2  Errors: 0  Failures: 0
                                              
    Cases: 3  Tried: 3  Errors: 0  Failures: 0
    Counts {cases = 3, tried = 3, errors = 0, failures = 0}
    *Lec2> 

-}            

-- Pattern matching on lists
isHi :: String -> Bool
isHi ['H', 'i'] = True
isHi _          = False

isGreeting :: String -> Bool
isGreeting "Hi"        = True
isGreeting "Hello"     = True
isGreeting "Bonjour"   = True
isGreeting "Guten Tag" = True
isGreeting _           = False   

isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton _   = False

isLong :: [a] -> Bool
isLong (x:y:xs) = True
isLong _        = False

{- --------------------------------------------------------------------
    Practise:
        Define a function, called listAdd, that, given a list of
        Ints returns their sum
        
        Step 1. Define test cases
        Step 2. Define the function signature
        Step 3. Implement the function
        Step 4. Run the tests
        Step 5. Refined and refactor, re-running the tests
    
-}     
-- Step 1
listAddTests :: Test
listAddTests = TestList [ listAdd [1,2,3] ~?= 6,
                          listAdd []      ~?= 0,
                          listAdd [10]    ~?= 10]

listAdd :: [Int] -> Int
listAdd []     = 0
listAdd [x]    = x
listAdd (x:xs) = x + listAdd xs

runLATests :: IO Counts
runLATests = runTestTT listAddTests

{-
    *Lec2> runLATests

    Cases: 3  Tried: 0  Errors: 0  Failures: 0
    Cases: 3  Tried: 1  Errors: 0  Failures: 0
    Cases: 3  Tried: 2  Errors: 0  Failures: 0
                                              
    Cases: 3  Tried: 3  Errors: 0  Failures: 0
    Counts {cases = 3, tried = 3, errors = 0, failures = 0}

-}                          
     
listAdd' :: [Int] -> Int
listAdd' = sum
     
runLATests' :: IO Counts
runLATests' = runTestTT (TestList [ listAdd' [1,2,3] ~?= 6,
                                   listAdd' []      ~?= 0,
                                   listAdd' [10]    ~?= 10] )    

{-
    *Lec2> runLATests'

    Cases: 3  Tried: 0  Errors: 0  Failures: 0
    Cases: 3  Tried: 1  Errors: 0  Failures: 0
    Cases: 3  Tried: 2  Errors: 0  Failures: 0
                                              
    Cases: 3  Tried: 3  Errors: 0  Failures: 0
    Counts {cases = 3, tried = 3, errors = 0, failures = 0}
    *Lec2> 

-}                                   

{- ----------------------------------------------------------
    Practise:
        Define a function, called listIncr, that, given a list
        of Ints, returns a new list where each number has been
        incremented.
        
        Step 1. Write test cases
        Step 2. Define the type signature
        Step 3. Implement the function
        Step 4. Run the tests
        Step 5. Refactor and re-run tests
-}         
-- Step 1        
listIncrTests :: Test
listIncrTests = 
 TestList [ listIncr [1,2,3] ~?= [2,3,4],
            listIncr [42]    ~?= [43],
            listIncr []      ~?= ([] :: [Int]) ]      

-- Step 2 & 3            
listIncr :: [Int] -> [Int]
listIncr (x:xs) = x + 1 : listIncr xs
listIncr _      = []

-- Step 4
runLITests :: IO Counts
runLITests = runTestTT listIncrTests

{-
    *Lec2> runLITests

    Cases: 3  Tried: 0  Errors: 0  Failures: 0
    Cases: 3  Tried: 1  Errors: 0  Failures: 0
    Cases: 3  Tried: 2  Errors: 0  Failures: 0
                                              
    Cases: 3  Tried: 3  Errors: 0  Failures: 0
    Counts {cases = 3, tried = 3, errors = 0, failures = 0}
    *Lec2> 
-}

-- Step 5
listIncr' :: [Int] -> [Int]
listIncr' = map (+1)

runLITests' :: IO Counts
runLITests' = runTestTT (
    TestList [ listIncr' [1,2,3] ~?= [2,3,4],
               listIncr' [42]    ~?= [43],
               listIncr' []      ~?= ([] :: [Int]) ] )
               
{-
    *Lec2> runLITests'

    Cases: 3  Tried: 0  Errors: 0  Failures: 0
    Cases: 3  Tried: 1  Errors: 0  Failures: 0
    Cases: 3  Tried: 2  Errors: 0  Failures: 0
                                              
    Cases: 3  Tried: 3  Errors: 0  Failures: 0
    Counts {cases = 3, tried = 3, errors = 0, failures = 0}
    *Lec2> 
-}               
         
-- IO actions are a new sort of value that describe an effect
-- on the world; the only way to execute an action (without ghci)
-- is to make it a value of "main"
         
-- example IO
-- compile: ghc -o hello -main-is Lec2 Lec2.hs
main :: IO ()
main = putStr "Hello World! \n"

-- actions just describe effects; we can pass them around like
-- any other value
-- note that this justs creates a pair holding two actions
-- it does not execute the actions
act2 :: (IO (), IO ())
act2 = (putStr "Hello", putStr "Hello")

-- if we want to do many actions we compose small actions
many :: IO ()
many = do putStr "Hello"
          putStr " World!"
          putStr "\n"
          
-- actions can also return values
query :: IO ()
query = do putStr "What is your name? "
           n <- getLine
           putStrLn ("Welcome to CIS 552 " ++ n)

-- Example of Testing Actions
numTest :: IO Counts
numTest = runTestTT (3 ~?= 4)

{-
    *Lec2> numTest

    Cases: 1  Tried: 0  Errors: 0  Failures: 0
                                              
    ### Failure:
    expected: 4
     but got: 3
    Cases: 1  Tried: 1  Errors: 0  Failures: 1
    Counts {cases = 1, tried = 1, errors = 0, failures = 1}
    *Lec2>

-}

dotest :: IO ()
dotest = do c <- runTestTT (3 ~?= 3)
            putStrLn (show c)
            
{-
    *Lec2> dotest

    Cases: 1  Tried: 0  Errors: 0  Failures: 0
                                              
    Cases: 1  Tried: 1  Errors: 0  Failures: 0
    Counts {cases = 1, tried = 1, errors = 0, failures = 0}
    *Lec2> 

-}           
            
-- functions are data, they can be passed around like any other data
-- if you have two functions, you can make a pair that will contain both
plus1 :: Int -> Int
plus1 x = x + 1

minus1 :: Int -> Int
minus1 x = x - 1

funp :: (Int->Int, Int->Int)
funp = (plus1, minus1)

-- or you can make a list of functions
funs :: [(Int -> Int)]
funs = [plus1, minus1]

-- this feature of functions allows us to write higher-order functions
-- that can take and/or return functions
doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

dtTests :: Test
dtTests = TestList [ doTwice plus1  4 ~?= 6,
                     doTwice minus1 5 ~?= 3]
 
runDTTests :: IO Counts
runDTTests = runTestTT dtTests

{-
    *Lec2> runDTTests

    Cases: 2  Tried: 0  Errors: 0  Failures: 0
    Cases: 2  Tried: 1  Errors: 0  Failures: 0
                                              
    Cases: 2  Tried: 2  Errors: 0  Failures: 0
    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
    *Lec2> 

-}
{-
    How doTwice is executed
    
        doTwice plus1 10 
     == plus1 (plus1 10)        {- unfold doTwice -} 
     == plus1 (10 + 1)          {- unfold plus1 -}
     == (10 + 1) + 1            {- unfold plus1 -}
     == 12                      {- old-school arithmetic -}    

-}
      
-- it can also be usefule to return functions as output
plusn :: Int -> (Int -> Int)
plusn n = f
    where f x = x + n
    
plus10 :: Int -> Int
plus10 = plusn 10

minus20 :: Int -> Int
minus20= plusn (-20)    
           
-- Partial Application
--      In regular arithmetic the minus operator is left-associative
--      so    2 - 1 - 1 == (2 - 1) - 1 == 0
--
--      In Haskell, the type operator (->) is 'right-associative'
--      so      Int -> Int -> Int == Int -> (Int -> Int)
--
--     A function that takes two ints is really a function that takes
--     a single Int and returns a function (Int -> Int)

plus :: Int -> Int -> Int
plus m n = m + n

-- we can partially apply the function by supplying a single argument
plusfive :: Int -> Int
plusfive = plus 5

pfivetest :: Test
pfivetest = plusfive 1000 ~?= 1005

runP5Test :: IO Counts
runP5Test = runTestTT pfivetest

doTwicePlus20 :: Int -> Int
doTwicePlus20 = doTwice (plus 20)

-- Anonymous functions
--      have no name i.e. \x -> x + 1
anonTests :: Test
anonTests = TestList [ (\x -> x + 1) 100 ~?= 101,
                       doTwice (\x -> x + 1) 100 ~?= 102 ]
                       
af1 = runTestTT anonTests
            
-- Infix operators and Sections
--      operator in parentheses can be used as infix operators
--      ($), the application operator, is one such
--      it has lowest precedence
--          ($) :: (a -> b) -> a -> b
--          f $ x = f x

inf1 = minus20 $ plus 30 32
inf2 = minus20 (plus 30 32)
inf3 = inf1 == inf2

-- any function can be used as an infix operator, just wrap it
-- in backticks 
anotherFive :: Int
anotherFive = 2 `plus` 3

threeThirties :: [Int]
threeThirties = 30 `clone` 3

-- partially applied infix operators are called 'sections'
anotherFour :: Int
anotherFour = doTwice (+2) 0

s1 = doTwice (1:) [2..5]

-- Polymorphism
--  doTwice is polymorphic as it works across different types
--  polymorphism allows for the reuse of an abstraction
--  of course, type signatures must match; can't use a function
--  with a different type signature using doTwice            
--  i.e. doTwice has the type signature
--              (a -> a) -> a -> a
--       can't pass it a function of type: (a -> b)
--
                       

