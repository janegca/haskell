{-# OPTIONS_GHC -Wall #-}
module W04H where

{-
    Week 04 - Higher-order programming and type inference
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/hw/
        04-higher-order.pdf

-}

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
        
-}    
    
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)
    
foldTree :: Ord a => [a] -> Tree a
foldTree = foldr insert Leaf

-- Ref:  see Discrete Math, Chapter 11 AVL Trees
--
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x tree@(Node h t1 d t2) =    
    if x < d then
        if (height newL) > (height t2) + 1
        then rotR (Node h newL d t2)
        else Node (h+1) newL d t2
    else if x > d then
        if (height newR) > (height t1) + 1
        then rotL (Node h t1 d newR)
        else Node (h+1) t1 d newR
    else tree                                   -- no change to tree
    where
        newL = insert x t1
        newR = insert x t2
                
height :: Tree a -> Integer
height Leaf           = 0
height (Node h _ _ _) = h
   
-- balances a tree that is right-heavy (inside or out)   
rotL :: Tree a -> Tree a   
rotL tree@(Node h t1 d t2@(Node _ yL _ yR)) =
    if (height yR) < (height yL)
    then easyLeft (Node h t1 d (easyRight t2))
    else easyLeft tree
rotL tree = tree    

-- balances a tree that is left-heavy (inside or out)    
rotR :: Tree a -> Tree a
rotR tree@(Node h t1@(Node h1 xL _ xR) d t2) =
    if (height xL) > (height xR)
    then easyRight (Node h (easyLeft t1) d t2)
    else easyRight tree
rotR tree = tree    

-- re-balance an 'outer right heavy' tree    
easyLeft :: Tree a -> Tree a
easyLeft (Node h t1 d (Node h1 yL a yR)) =
    Node h1 (Node (h1-1) t1 d yL) a yR
easyLeft tree = tree

-- re-balance an 'outer left heavy' tree
easyRight :: Tree a -> Tree a
easyRight (Node h (Node h1 xL a xR) d t2) =
    Node h1 xL a (Node (h1-1) xR d t2)
easyRight tree = tree    
        
-- test inserts 

ta, tb, tc, td, te, tf, tg :: Tree String
ta = insert "A" Leaf
tb = insert "B" ta
tc = insert "C" tb
td = insert "D" tc
te = insert "E" td
tf = insert "F" te
tg = insert "G" tf
   
-- TODO: HEIGHTS ARE MESSED UP SO BALANCING IS OFF   
testTree :: Tree Char
testTree = foldTree "ABCDEFGHIJ"
    