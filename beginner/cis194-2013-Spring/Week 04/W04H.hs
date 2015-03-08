{-# OPTIONS_GHC -Wall #-}
module W04H where

{-
    Week 04 - Higher-order programming and type inference
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/hw/
        04-higher-order.pdf

-}
import Data.List ( (\\) )  -- list difference operator

{-
    Exercise 1: Wholemeal programming

    Reimplement each of the following functions in a more idiomatic
    Haskell style. Use wholemeal programming practices, breaking each
    function into a pipeline of incremental transformations to an entire
    data structure. Name your functions fun1’ and fun2’ respectively.

    1.  fun1 :: [Integer] -> Integer
        fun1 [] = 1
        fun1 (x:xs)
            | even x = (x - 2) * fun1 xs
            | otherwise = fun1 xs

    2.  fun2 :: Integer -> Integer
        fun2 1 = 0
        fun2 n | even n = n + fun2 (n ‘div‘ 2)
            | otherwise = fun2 (3 * n + 1)

    Hint: For this problem you may wish to use the functions iterate
    and takeWhile. Look them up in the Prelude documentation to see
    what they do.
    
    iterate f x  - returns a repeated list of f applied to x
    takeWhile p  - takes items from a list while the predicate p is True

-}
fun1 :: [Integer] -> Integer
fun1 []      = 1
fun1 (x:xs)  | even x    = (x - 2) * fun1 xs
             | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (+ (-2)) . filter even
             
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)    
       
-- Ref: 
--  https://github.com/potatosalad/cis194/blob/master/src/Cis194/Week4.hs 
--      
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate g
    where g x = if even x then div x 2 else x*3 + 1
    
{-
    
    Exercise 2: Folding with trees

    Recall the definition of a binary tree data structure. The height of
    a binary tree is the length of a path from the root to the deepest
    node. For example, the height of a tree with a single node is 0; the
    height of a tree with three nodes, whose root has two children, is 1;
    and so on. A binary tree is balanced if the height of its left and 
    right subtrees differ by no more than 1, and its left and right 
    subtrees are also balanced.
    
    You should use the following data structure to represent binary
    trees. Note that each node stores an extra Integer representing the
    height at that node.
    
        data Tree a = Leaf
                    | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)
    
    For this exercise, write a function    

        foldTree :: [a] -> Tree a
        foldTree = ...

    which generates a balanced binary tree from a list of values using
    foldr.
    
    For example, one sample output might be the following:

        foldTree "ABCDEFGHIJ" ==
            Node 3
                (Node 2
                    (Node 0 Leaf ’F’ Leaf)
                     ’I’
                    (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
                ’J’
                (Node 2
                    (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
                    ’H’
                    (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))
                    
    also visualized as: 

                                    J             (3)                             
                                 /     \
                                I       H         (2)
                               / \     /  \
                              F   C   G    E      (1)
                                 /   /    /
                                B   A    D        (0)
                                
                                
    Your solution might not place the nodes in the same exact order,
    but it should result in balanced trees, with each subtree having a
    correct computed height.
        
    Ref: https://stackoverflow.com/questions/16157203/
              build-balanced-binary-tree-with-foldr
              
    Note: originally tried to build and 'ordered' balanced tree, which
          wasn't required; trick was to use one subtree to measure the
          overall height of the tree and to only increase the tree height
          when adding a node when both subtrees are of equal height
-}    
    
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)
    
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node n t1 d t2) 
    | h1 < h2   = Node n (insert x t1) d t2
    | h1 > h2   = Node n t1 d t2n            
    | otherwise = Node (h + 1) t1 d t2n
    where                
        height :: Tree a -> Integer
        height Leaf           = -1  -- if 0 no diff between Node with
        height (Node s _ _ _) = s   -- with 2 leaves and a single Leaf
        
        h1  = height t1
        h2  = height t2
        t2n = insert x t2 -- use the right tree to measure overall height
        h   = height t2n
           
testTree :: Tree Char
testTree = foldTree "ABCDEFGHIJ"

{-
    Output of 'testTree'
    
    Node 3 
        (Node 2 (Node 0 Leaf 'C' Leaf) 
             'H' 
                (Node 1 Leaf 'F' (Node 0 Leaf 'B' Leaf)))
        'J' 
        (Node 2 (Node 1 Leaf 'E' (Node 0 Leaf 'A' Leaf)) 
             'I' 
                (Node 1 Leaf 'G' (Node 0 Leaf 'D' Leaf)))
               
               
                        J          3
                      /   \
                     H     I       2
                    / \   / \
                   C   F  E  G     1
                        \  \  \
                        B   A  D   0
    
-}
{-
    Exercise 3 - More folds!

    1.  Implement a function
            xor :: [Bool] -> Bool
        
        which returns True if and only if there are an odd number of True
        values contained in the input list. It does not matter how many
        False values the input list contains. For example,

            xor [False, True, False]              == True
            xor [False, True, False, False, True] == False
        
        Your solution must be implemented using a fold.
        
    2.  Implement map as a fold. That is, complete the definition
        
            map’ :: (a -> b) -> [a] -> [b]
            map’ f = foldr ...
        
        in such a way that map’ behaves identically to the standard map
        function.        
        
    3. (Optional) Implement foldl using foldr. That is, complete the
        definition
        
            myFoldl :: (a -> b -> a) -> a -> [b] -> a
            myFoldl f base xs = foldr ...
        
        in such a way that myFoldl behaves identically to the standard
        foldl function.
        
        Hint: Study how the application of foldr and foldl work out:
        foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
        foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn        

-}    
-- Solution Ref: https://github.com/pdswan/cis194/blob/master/hw4/Hw4.hs
xor :: [Bool] -> Bool
xor = foldr (\x y -> if x then (not y) else y) False

ex3a :: Bool
ex3a =   xor [False, True, False]              == True
     &&  xor [False, True, False, False, True] == False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []
     
ex3b :: Bool
ex3b =  m1 == m2
    where
        m1, m2 :: [Integer]
        m1 = map' (+1) [1..5]
        m2 = map  (+1) [1..5]
        
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (nextStepFn f) id xs $ base
    where
        nextStepFn :: (a -> b -> a) -> b -> (a -> a) -> (a -> a)
        nextStepFn iter item g = g . (flip iter item)
   
ex3c :: Bool   
ex3c = fold == 5
    where 
        fold :: Integer
        fold = myFoldl max 5 [1,2,3,4]       
        
{-

    Exercise 4: Finding primes

    Read about the Sieve of Sundaram. Implement the algorithm using 
    function composition. Given an integer n, your function should
    generate all the odd prime numbers up to 2n + 2.
    
        sieveSundaram :: Integer -> [Integer]
        sieveSundaram = ...
    
    To give you some help, below is a function to compute the Cartesian
    product of two lists. This is similar to zip, but it produces all
    possible pairs instead of matching up the list elements. For example,
    cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]
    
    It’s written using a list comprehension, which we haven’t talked about
    in class (but feel free to research them).
    
        cartProd :: [a] -> [b] -> [(a, b)]
        cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-}        
-- this works but need to (a) use cartProd and (b) higher order
-- functions 
-- Ref: https://stackoverflow.com/questions/16246456/
--              sieve-of-sundaram-list-comprehension
--
sieveSundaram :: Integer -> [Integer]
sieveSundaram n =  [ 2*x+1 | x <- [1..n], not (elem x excl)]
    where excl =  filter (< n) 
                    [ i + j + 2 * i * j | i <- [1..n], j <- [i..n]]

-- above re-written to use 'pipeline' of higher order functions                    
sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = map ((+1) . (*2)) -- values that satisfy the algo
                  ([1..n] -- only check values that are
                      \\  -- not in the list of values to exclude
                      ( filter (<n) 
                      . map (\(i,j) -> i + j + 2 * i * j) 
                      $ cartProd [1..n] [1..n]))  -- (i,j) pairs                     
                      
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
