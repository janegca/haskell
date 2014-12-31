-- Informatics 1 - Functional Programming 
-- Tutorial Review 8 - General Review
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/#lectures

import Test.QuickCheck
import Data.Char

{-
    In this tutorial we will quickly go through all of the major topics 
    of the course. Since this is very general revision, you should not
    have any trouble doing any of these exercises. If you do find any of 
    them challenging, perhaps you should focus on revising that area.

-}
{- 1. Functions

    Define a function f1 that given two numbers will tell us whether 
    the first is divisible by the second. You should provide an
    appropriate type declaration for this function.

-}
f1 :: Int -> Int -> Bool
f1 x y = mod x y == 0

{- 2. Recursion

    Define a recursive function f2 that given two lists of numbers will 
    give the product of all the numbers in the first list that are 
    divisible by the corresponding number in the second list.
    
        f2 [21, 34, 22, 9] [2, 2, 4, 3] == 34 * 9 == 306
    
    Your function should give a meaningful error if the lengths of the
    lists do not match.
    
    You should also have an idea how you would approach this problem 
    using both list comprehension and higher order functions.

-}
f2 :: [Int] -> [Int] -> Int
f2 [] []         = 1   
f2 (x:xs) (y:ys) | f1 x y    = x * f2 xs ys
                 | otherwise = f2 xs ys
f2 _ _ = error("incompatible lengths")  

f2' :: [Int] -> [Int] -> Int
f2' xs ys = product [ x | (x,y) <- zip xs ys, f1 x y]           

f2'' :: [Int] -> [Int] -> Int
f2'' xs ys = product . map fst . filter (uncurry f1) $ (zip xs ys)

test2 fn = fn [21, 34, 22, 9] [2, 2, 4, 3] == 306

testq2 = test2 f2 && test2 f2' && test2 f2''

{- 3. Characters and Strings

    Write a function f3 that given a string containing only letters 
    and spaces, will return a list of the words in the string that 
    begin with an uppercase letter. For example:
    
            f3 "this is a String of Words" == ["String","Words"]
    
    A library function might come in handy, but you don't need to use 
    it.

-}
f3 :: String -> [String]
f3 = filter (isUpper . head) . words

f3' :: String -> [String]
f3' xs = [ w | w <- words xs, isUpper (head w) ]

f3'' :: String -> [String]
f3'' str = g (words str)
    where 
        g ( (w:ws) : xs  ) 
            | isUpper w  = (w:ws) : g xs
            | otherwise  = g xs
        g [] = []

test3 fn = fn "this is a String of Words" == ["String","Words"]

testq3 = test3 f3 && test3 f3' && test3 f3''

{- 4. List Comprehensions

    Given two lists of integers, use list comprehension to define a
    function f4 that returns a list of all the possible sums of a
    number from the first list and a number from the second list:

        f4 [1, 2] [3, 4]   == [4,5,5,6]
        f4 [1,2] [5,6,9,2] == [6,7,10,3,7,8,11,4]

    [ MANIPULATING TWO LISTS ]
-}
f4 :: [Int] -> [Int] -> [Int]
f4 xs ys = [ i + j | i <- xs, j <- ys ]

-- provided  higher order function solution
f4' :: [Int] -> [Int] -> [Int]
f4' as bs = concat (map (\a -> map (\b -> a + b) bs) as)

-- provided recursive solution
f4'' :: [Int] -> [Int] -> [Int]
f4'' [] _ = []
f4'' (a:as) bs = (f bs) ++ (f4'' as bs)
  where f :: [Int] -> [Int]
        f [] = []
        f (x:xs) = (a + x) : f xs

test4 fn = fn [1, 2] [3, 4]   == [4,5,5,6] 
        && fn [1,2] [5,6,9,2] == [6,7,10,3,7,8,11,4]

testq4 = test4 f4 && test4 f4' && test4 f4''        
       
{- 5. List Processing with Higher Order Functions

    Using foldr give definitions of map' and filter' that behave like
    their prelude counterparts.
    
    Also think how you would solve 2, 3 and 4 above with higher order 
    functions.
-}                                  

-- provided solutions
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\a b -> if f a then a : b else b) []

{- 6. Algebraic Data Types

    Write a definition for the type 'TwoTree a' where a branch can have 
    0, 1 or 2 children, together with a node label of type a. 
    
    Write a function 
            treeFold :: (a -> b -> b) -> b -> TwoTree a -> b
            
    that will work similarly like foldr but for your TwoTree type. 
    That is, the function should go through all the nodes of the tree 
    and apply the function passed to treeFold on all of these 
    (the order in which you go through them sn't specified).

-}
-- provided solution

data TwoTree a = Leaf a 
               | Branch1 a (TwoTree a) 
               | Branch2 a (TwoTree a) (TwoTree a)
               deriving (Show)

treeFold :: (a -> b -> b) -> b -> TwoTree a -> b
treeFold f a (Leaf e)      = f e a
treeFold f a (Branch1 e t) = f e (treeFold f a t)
treeFold f a (Branch2 e t1 t2) = f e (treeFold f (treeFold f a t1) t2)

{- 7. Type Classes

    Create a type class called StructurallySimilar which defines a 
    function
    
        similar :: a ->  a -> Bool 
    
    that checks if the two arguments have the same structure.
    
    Make your TwoTree an instance of this class. Two TwoTrees are similar 
    iff each node has the same number of children as the corresponding 
    node in the other tree.
    
    Hint: Lists would be similar iff they had the same number of elements. 
          We can do this by:
            instance StructurallySimilar [a] 
          
          where  similar l1 l2 = length l1 == length l2 
          
          Then also make TwoTree an instance of the Show class.

-}
-- provided solution
class StructurallySimilar a where
  similar :: a -> a -> Bool
  
instance StructurallySimilar (TwoTree a) where
  similar (Leaf _) (Leaf _)             = True
  similar (Branch1 _ t1) (Branch1 _ t2) = similar t1 t2
  similar (Branch2 _ t1a t1b) 
          (Branch2 _ t2a t2b)           = similar t1a t2a && 
                                          similar t1b t2b
  similar _ _                           = False
  
instance StructurallySimilar [a] where
  similar a b = length a == length b

{- 8. Testing

    Import Test.QuickCheck into your file. Write tests that verify if the 
    following versions of the well known library functions work correctly
    (if possible, try to think of tests other then just checking if they
    behave identically to their prelude counterparts). Fix any bugs that
    you find:
   
-}
product' [] = 1
product' (x:xs) = x * product' xs

prop_product' :: [Int] -> Bool
prop_product' xs = product' xs == product xs

-- provided solution
prop_product xs = foldr (*) 1 xs  == product' xs

-- less than redefinition
x < y = (x >= y)

-- quickCheck fails, if x and y are equal, Prelude returns False
-- this definition returns True
prop_lt :: Int -> Int -> Bool
prop_lt x y = x Main.< y == x Prelude.< y

-- provided solution - fails quickCheck
-- This one is really obviously wrong
prop_lt' :: Int -> Int -> Bool
prop_lt' x y = (y >= x) `xor` (x Main.< y)
  where 
    xor True  a = not a
    xor False a = a
    
