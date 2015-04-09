{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Lec3 where
{-
    CIS 552: Advanced Programming (2015)
    Lecture 3 - Higher-Order Programming Patterns
    
    Source:
    https://www.seas.upenn.edu/~cis552/lectures/stub/Lec3.html

-}
import Prelude hiding (map, foldr, filter, pred, sum, product)
import Data.Char
import Test.HUnit

-- Polymorphic Data Structures
--      Polymorphic functions can work with different kinds of values
--      Most of the standard list functions are 'generic' in that
--      they will work over any list, regardless of element type
len :: [a] -> Int
-- will work over any list, regardless of the type of list element
len [] = 0
len (_:xs) = 1 + len xs

l1 = len [1.1, 2.2, 3.3, 4.4]
l2 = len "mmm donuts!"
l3 = len [[],[1],[1,2],[1,2,3]]

-- Polymorphism allows for the creation of 'higher-order' functions
-- it allows for the abstraction and reuse of computing patterns
--
-- Iteration Pattern

toUpperString :: String -> String
toUpperString [] = []
toUpperString (x:xs) = toUpper x : toUpperString xs

ex1 = toUpperString "hello world"   -- "HELLO WORLD"

type XY      = (Double, Double)
type Polygon = [XY]

shiftXY :: XY -> XY -> XY
shiftXY (dx, dy) (x, y) = (dx + x, dy + y)

ex2 = shiftXY (1,2) (2,3)           -- (3.0, 5.0)

shiftPoly :: XY -> Polygon -> Polygon
shiftPoly _ []       = []
shiftPoly d (xy:xys) = shiftXY d xy : shiftPoly d xys

ex3 = shiftPoly (1,2) [(0,0),(1,1),(2,2)]  
        -- [(1.0,2.0),(2.0,3.0),(3.0,4.0)]
        
-- the function 'toUpperString' and 'shiftPoly' share the same pattern
-- they walk over a list applying a function to each element
-- this pattern can be abstracted to a higher-order function: map
map :: (a -> a) -> [a] -> [a]
map _ []     = []
map f (x:xs) = f x : map f xs

-- we can now rewrite the two functions to use 'map'
toUpperString' :: String -> String
toUpperString' = map toUpper

shiftPoly' :: XY -> Polygon -> Polygon
shiftPoly' d = map (shiftXY d)

ex4 = toUpperString' "hello world" == ex1
ex5 = shiftPoly' (1,2) [(0,0),(1,1),(2,2)]  == ex3     

-- better yet, use tests to check refactoring didn't break things
testMap = runTestTT $ TestList $ 
  [     toUpperString' "abc" 
    ~?= toUpperString "abc"
  , 
        shiftPoly' (0.5,0.5) [(1,1),(2,2),(3,3)] 
    ~?= shiftPoly (0.5,0.5) [(1,1),(2,2),(3,3)] ] 
    
{-

    *Lec3> testMap

    Cases: 2  Tried: 0  Errors: 0  Failures: 0
    Cases: 2  Tried: 1  Errors: 0  Failures: 0
                                              
    Cases: 2  Tried: 2  Errors: 0  Failures: 0
    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
    *Lec3> 

-} 
-- Note that we dropped the parameters from the definitions
--      this is because we used 'partial application'
--      generally: f x = ex is the same as f = e as long as
--      'x' is not used in 'e'

-- the listIncr function was another example of the iteration pattern
-- that can be replaced with map
listIncr :: [Int] -> [Int]
listIncr []     = []
listIncr (x:xs) = (x+1) : listIncr xs

listIncr' :: [Int] -> [Int]
listIncr' = map (+1)

ex6 = listIncr [0..3] == listIncr' [0..3]

-- 'Folding' is another computation pattern

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs

product :: [Int] -> Int
product []     = 1
product (x:xs) = x * product xs

ex7 = sum [0..3]            -- 6
ex8 = product [1..3]        -- 6

-- both have the same pattern; an empty list computes to a 'base'
-- value and a function is used to combine elements; the pattern
-- can be reduced to a 'foldr' function
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b []     = b
foldr f b(x:xs) = x `f` (foldr f b xs)

-- we can now rewrite sum and product to use foldr
sum', product' :: [Int] -> Int
sum'     = foldr (+) 0
product' = foldr (*) 1

foldrTest = runTestTT $ TestList [
              sum' [1,2,3] ~?= sum [1,2,3],
              product' [1,2,3] ~?= product [1,2,3] 
            ]
{-            
    *Lec3> foldrTest

    Cases: 2  Tried: 0  Errors: 0  Failures: 0
    Cases: 2  Tried: 1  Errors: 0  Failures: 0
                                              
    Cases: 2  Tried: 2  Errors: 0  Failures: 0
    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
    *Lec3>     
-}        
{-
    Walking through an execution of foldr
    
            foldr f base [x1,x2,...,xn
         == f x1 (foldr f base [x2,...,xn])     {- unfold foldr -}
         == f x1 (f x2 (foldr f base [...,xn])  {- unfold foldr -}
         == f x1 (f xs (... (f xn base)))       {- unfold foldr -}
         
    easy to see that the structure mirrors the list structure with
    'f' replacing ':' and base replacing []
    
            x1 : x2 : ... : xn : []
            x1 f xs f ... f xn f base

-}
-- we can use foldr to eliminate recursion in the len function
len' :: [a] -> Int
len' = foldr (\x -> (+1)) 0  -- why an anon function??
                             -- because we're not manipulating 'x'

ex9 = len' [[],[1],[1,2],[1,2,3]] == len [[],[1],[1,2],[1,2,3]]

-- use foldr to eliminate recursion from the factorial function
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' :: Int -> Int
factorial' n = foldr (*) 1 [1..n]

ex10 = factorial 8 == factorial' 8

-- can we write filter using foldr?
filter :: (a -> Bool) -> [a] -> [a]
filter pred = foldr (\x y -> if pred x then x:y else y) []

filterTests :: Test
filterTests = TestList 
     [ filter (>10) [1..20] ~?= [11..20],
       filter (\l -> sum l <= 42) [ [10,20], [50,50], [1..5] ]
         ~?= [[10,20],[1..5]] ]
         
runFTests = runTestTT filterTests
         
{-
    *Lec3> runFTests

    Cases: 2  Tried: 0  Errors: 0  Failures: 0
    Cases: 2  Tried: 1  Errors: 0  Failures: 0
                                              
    Cases: 2  Tried: 2  Errors: 0  Failures: 0
    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
    *Lec3>

-}







