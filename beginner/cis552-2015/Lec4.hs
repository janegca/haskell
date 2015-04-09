{-# OPTIONS -fno-warn-type-defaults -fwarn-incomplete-patterns #-}
module Lec4 where
{-
    CIS 552: Advanced Programming (2015)
    Lecture 4 - User-defined data types
    
    Source:
    https://www.seas.upenn.edu/~cis552/lectures/stub/Lec4.html

-}
import Prelude hiding (Maybe,Just,Nothing,Either,Left,Right)
import Test.HUnit

-- user-defined type for days of the week
-- Monday, Tuesday, Wednesday, etc are 'constructors'
-- that take no arguments
data Day = Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         | Sunday
  deriving (Show, Eq)
  
-- a function defined on Day using pattern matching  
nextWeekday :: Day -> Day
nextWeekday Monday    = Tuesday
nextWeekday Tuesday   = Wednesday
nextWeekday Wednesday = Thursday
nextWeekday Thursday  = Friday
nextWeekday Friday    = Monday
nextWeekday Saturday  = Sunday
nextWeekday Sunday    = Monday  

twoBusinessDays :: Day -> Day
twoBusinessDays Monday    = Wednesday
twoBusinessDays Tuesday   = Thursday
twoBusinessDays Wednesday = Friday
twoBusinessDays Thursday  = Monday
twoBusinessDays _         = Tuesday

-- better solution
twoBusinessDays' :: Day -> Day
twoBusinessDays' = nextWeekday . nextWeekday

-- datatypes can also carry values
-- here, Circle is specified by two double values,
--       Rectangle, by four double values
-- Circle and Rectangle are constructors that take arguments
-- they have 'type signatures' similar to functions
-- i.e. Circle :: Double -> Double -> Double -> Shape
--      Rectangle: Double -> Double -> Double -> Double -> Shape
{-
data Shape =
   Circle    Double Double Double
 | Rectangle Double Double Double Double
 
area :: Shape -> Double
area (Circle x y r) =  pi * r * r
area (Rectangle x1 y1 x2 y2) = (x2 - x1) * (y2 - y1)
-}
-- better way to define Shape
type Radius = Double
data Point  = Point Double Double

data Shape = Circle    Point Radius
           | Rectangle Point Point
           
area :: Shape -> Double
area (Circle _ r) = pi * r * r
area (Rectangle (Point x1 y1) (Point x2 y2)) =
            (x2 - x1) * (y2 - y1)

-- data types can be recursive
-- IntListNE represents a 'non-empty' list of integers
data IntListNE = ISingle Int
               | ICons Int IntListNE
             
-- the list [1,2,3] is represented as             
oneTwoThree :: IntListNE
oneTwoThree = ICons 1 (ICons 2 (ISingle 3))

-- which is comparable to the way built-in lists are constructed
oneTwoThree' :: IntListNE
oneTwoThree' = 1 `ICons` (2 `ICons` (ISingle 3))

-- we can defined functions by recursion as well
sumOfIntListNE :: IntListNE -> Int
sumOfIntListNE (ISingle x)  = x
sumOfIntListNE (ICons x xs) = x + sumOfIntListNE xs

testSumIL = "sumOfIntListNE" ~: sumOfIntListNE oneTwoThree ~?= 6
runTestSumIL = runTestTT testSumIL

-- we can also define Polymorphic Datatypes
-- Maybe defintion from Prelude
data Maybe a = Nothing | Just a

justThree :: Maybe Int
justThree = Just 3

noInt :: Maybe Int
noInt = Nothing

justTrue :: Maybe Bool
justTrue = Just True
        
-- the definiton of Either from the Prelude
data Either a b = Left a | Right b deriving Show

safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "You can't divide by zero, silly."
safeDiv x y = Right $ x `div` y

ex1 = safeDiv 4 2
ex2 = safeDiv 3 0

-- Trees
-- defining a tree with internal nodes
data Tree a = Leaf 
            | Branch a (Tree a) (Tree a)
    deriving (Eq, Show)
    
{-
    Example Tree
    
          5
        /   \
       2     9
      / \     \
     1   4     7    

-}    
exTree :: Tree Int
exTree = Branch 5 (Branch 2 (Branch 1 Leaf Leaf) (Branch 4 Leaf Leaf))
                  (Branch 9 Leaf (Branch 7 Leaf Leaf))    

-- and we can write simple functions on trees using recursion
treePlus :: Tree Int -> Int -> Tree Int
treePlus Leaf  n            = Branch n Leaf Leaf
treePlus (Branch x t1 t2) n = Branch (x+n) t1 t2

testTreePlus = "treePlus" ~:  treePlus (Branch 2 Leaf Leaf) 3 
                          ~?= (Branch 5 Leaf Leaf)
runTestTP = runTestTT testTreePlus                          

infixOrder :: Tree a -> [a]
infixOrder Leaf             = []
infixOrder (Branch x t1 t2) = (infixOrder t1) ++ [x] ++ (infixOrder t2)

testInfixOrder = "infixOrder" ~: infixOrder exTree ~?= [1,2,4,5,9,7]
runTestIFO = runTestTT testInfixOrder

-- but should really implement higher-order functions for trees
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf             = Leaf
treeMap f (Branch x t1 t2) = Branch (f x) (treeMap f t1) (treeMap f t2)

treeIncr :: Tree Int -> Tree Int
treeIncr = treeMap (+1)

testTreeIncr = "treeIncr" ~: treeIncr (Branch 1 (Branch 2 Leaf Leaf) Leaf) ~?= 
                                      (Branch 2 (Branch 3 Leaf Leaf) Leaf) 
runTestTI = runTestTT testTreeIncr

treeFold :: (a -> b -> b -> b) -> b -> Tree a -> b
treeFold _ b Leaf             = b
treeFold f b (Branch x t1 t2) = 
    f x (treeFold f b t1) (treeFold f b t2)
    
-- re-implement infoxOrder to use treeMap
infixOrder' :: Tree a -> [a]
infixOrder' = treeFold (\x t1 t2 -> t1 ++ [x] ++ t2) []
    
testInfixOrder' = "infixOrder'" ~: infixOrder' exTree ~?= [1,2,4,5,9,7]
runTestIFO' = runTestTT testInfixOrder'

-- and other orderings are as easily written
prefixOrder :: Tree a -> [a]
prefixOrder = treeFold (\x t1 t2 -> x : t1 ++ t2) []

postfixOrder :: Tree a -> [a]
postfixOrder = treeFold (\x t1 t2 -> t1 ++ t2 ++ [x]) []

testPrefixOrder  = "prefixOrder" ~: prefixOrder exTree ~?= [5,2,1,4,9,7]
testPostfixOrder = "postfixOrder" ~: postfixOrder exTree ~?= [1,4,2,7,9,5]

runTestPreOrd  = runTestTT testPrefixOrder
runTestPostOrd = runTestTT testPostfixOrder

main :: IO ()
main = do 
  -- run all tests
  runTestTT $ TestList [
    testSumIL,
    testTreePlus,
    testInfixOrder,
    testTreeIncr,
    testInfixOrder',
    testPrefixOrder,
    testPostfixOrder ]
  return ()

    