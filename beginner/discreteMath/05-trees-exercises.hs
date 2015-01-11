-- Chapter 5 - Trees - Exercises
--
-- Reference: 'Discrete Mathematics Using a Computer' by John O'Donnell,
--             Cordelia Hall, and Rex Page

import Tree05

{-
    Exercise 1
    
    Define a Haskell data type Tree1 for a tree that contains a character
    and an integer in each node, along with exactly three subtrees
-}
data Tree1 = Tree1Leaf
           | Tree1Node Char Integer Tree1 Tree1 Tree1
            
{-
    Exercise 2
    
    Define a Haskell data type Tree2 for a tree that contains an integer
    in each node, and that allows each node to have any number of subtrees
-}            
data Tree2 = Tree2Leaf
           | Tree2Node Integer [Tree2]
           
{-
    Exercise 3
    
    Calculate the inorder traversal of tree3
    
        inorder tree3
    ==> inorder Node 4 (Node 2 (Node 1 Leaf Leaf)
                               (Node 3 Leaf Leaf))
                       (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf))
                               (Node 8 Leaf Leaf))
    ==> inorder (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
        ++   [4]
        ++ inorder (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf)
        
    ==> inorder (Node 1 Leaf Leaf) ++ [2] ++ inorder (Node 3 Leaf Leaf)
        ++ [4]
        ++ inorder (Node 5 Leaf (Node 6 Leaf Lear)) 
            ++ [7] 
            ++  inorder (Node 8 Leaf Leaf)
            
    ==> [1] ++ [2] ++ [3] ++ [4] 
            ++ Leaf ++ [5] ++ inorder (Node 6 Leaf Lear)
            ++ [7] ++ [8]
            
    ==> [1, 2, 3, 4, 5] ++ [6] ++ [7, 8]
    ==> [1, 2, 3, 4, 5, 6, 7, 8]

-}           
{-
    Exercise 4
    
    Suppose that a tree has type BinTree a, and we have a function
        f :: a -> b. 
        
    Write a new traversal function 
        inorder f :: (a->b) -> BinTree a -> [b] 
        
    that traverses the tree using inorder, but it applies f
    to the data value in each node before placing the result in the list. 
    For example, 
        inorder tree6 produces [1, 2, 3, 4, 5, 6, 7], but 
        inorderf (2*) tree6 produces [2, 4, 6, 8, 10, 12, 14].    

-}
inorderf :: (a->b) -> BinTree a -> [b]
inorderf f BinLeaf           = []
inorderf f (BinNode x t1 t2) = inorderf f t1 ++ [f x] ++ inorderf f t2

t4 = inorder tree6 == [1..7] 
  && inorderf (*2) tree6 == [2,4..14]
  
{-
    Exercise 5
    
    Define two trees of size seven, one with the largest possible height
    and the other with the smallest possible height.
-}  
tree5a :: BinTree Integer
tree5a = BinNode 1
            BinLeaf
            (BinNode 2
                BinLeaf
                (BinNode 3 
                    BinLeaf
                    (BinNode 4
                        BinLeaf
                        (BinNode 5
                            BinLeaf
                            (BinNode 6 
                                BinLeaf
                                (BinNode 7 BinLeaf BinLeaf))))))

tree5b :: BinTree Integer
tree5b = BinNode 4
            (BinNode 2 
                (BinNode 1 BinLeaf BinLeaf)
                (BinNode 3 BinLeaf BinLeaf))
            (BinNode 6 
                (BinNode 5 BinLeaf BinLeaf)    
                (BinNode 7 BinLeaf BinLeaf))
                
{-
    Exercise 6
    
    Suppose that the last equation of the function balanced were
    changed to the following: 
        balanced (BinNode x t1 t2) = balanced t1 && balanced t2 
        
    Give an example showing that the modified function returns True for 
    an unbalanced tree.
-}                
balanced' :: BinTree a -> Bool
balanced' BinLeaf           = True
balanced' (BinNode x t1 t2) = balanced' t1 && balanced' t2

t6 = balanced' tree5a      -- returns True

{-
    Exercise 7
    
    Suppose that the last equation of the function balanced were
    changed to the following: 
        balanced (BinNode x t1 t2) = height t1 == height t2 
        
    Give an example showing that the modified function
    returns True for an unbalanced tree.
-}

balanced'' :: BinTree a -> Bool
balanced'' BinLeaf           = True
balanced'' (BinNode x t1 t2) = height t1 == height t2

tree7a :: BinTree Integer
tree7a = BinNode 1 BinLeaf BinLeaf

t7 = balanced'' tree7a  -- is a binary tree with only
                        -- a root unbalanced or balance??
                        
{-
    Exercise 8
    
    Define a function 'mapTree' that takes a function and applies it to
    every node in the tree, returning a new tree of results. The type should
    be 
            mapTree :: (a->b) -> BinTree a -> BinTree b. 
            
    This function is analogous to map, which operates over lists.    
-}     
mapTree :: (a->b) -> BinTree a -> BinTree b
mapTree f BinLeaf           = BinLeaf
mapTree f (BinNode x t1 t2) = BinNode (f x) (mapTree f t1) (mapTree f t2)

ex8 = mapTree (*2) tree6

{-
    Exercise 9
    
    Write concatTree, a function that takes a tree of lists and concatenates
    the lists in order from left to right. For example,
            concatTree (Node [2] (Node [3,4] Tip Tip)
                                 (Node [5] Tip Tip))
            ==> [3,4,2,5]
-}
concatTree :: BinTree [a] -> [a] 
concatTree BinLeaf           = []
concatTree (BinNode xs t1 t2) = concatTree t1 ++ xs ++ concatTree t2                 

tree9a = BinNode [2] (BinNode [3,4] BinLeaf BinLeaf)
                     (BinNode [5] BinLeaf BinLeaf)
                     
concatTree' :: BinTree [a] -> [a]
concatTree' = concat . inorder                     

ex9  = concatTree tree9a   
ex9a = concatTree' tree9a               
                
{-
    Exercise 10
    
    Write zipTree, a function that takes two trees and pairs each of
    the corresponding elements in a list. 
    
    Your function should return Nothing if the two trees do not have the 
    same shape. For example,
        zipTree (Node 2 (Node 1 Tip Tip) (Node 3 Tip Tip))
                (Node 5 (Node 4 Tip Tip) (Node 6 Tip Tip))
        ==> Just [(1,4),(2,5),(3,6)]

-}           
zipTree :: BinTree a -> BinTree b -> Maybe [(a,b)]
zipTree t1 t2 = if size t1 == size t2 && height t1 == height t2
                then Just (zt t1 t2)
                else Nothing
    where 
        zt BinLeaf BinLeaf = []
        zt (BinNode n1 x1 x2) (BinNode n2 y1 y2) =
           zt x1 y1 ++ [(n1,n2)] ++ zt x2 y2          
           
tree10a = BinNode 2 (BinNode 1 BinLeaf BinLeaf) (BinNode 3 BinLeaf BinLeaf)
tree10b = BinNode 5 (BinNode 4 BinLeaf BinLeaf) (BinNode 6 BinLeaf BinLeaf)

ex10  = zipTree tree10a tree10b
ex10a = zipTree tree5 tree6

{-
    Exercise 11
    
    Write zipWithTree, a function that is like zipWith except that
    it takes trees instead of lists. 
    
    The first argument is a function of type a->b->c, the second argument
    is a tree with elements of type a,and the third argument is a tree 
    with elements of type b. The function returns a list with type [c].

-}
zipWithTree :: (a->b->c) -> BinTree a -> BinTree b -> [c]
zipWithTree f BinLeaf _ = []
zipWithTree f _ BinLeaf = []
zipWithTree f (BinNode n1 x1 x2) (BinNode n2 y1 y2)
    = zipWithTree f x1 y1 ++ [f n1 n2] ++ zipWithTree f x2 y2

ex11a = zipWithTree (+) tree7 tree8         -- [2,5]
ex11b = zipWithTree (*) tree10a tree10b     -- [4,10,18]

{-
    Exercise 12
    
    Write appendTree, a function that takes a binary tree and a
    list, and appends the contents of the tree (traversed from left to 
    right) to the front of the list. For example,
    
        appendTree (BinNode 2 (BinNode 1 BinLeaf BinLeaf)
                              (BinNode 3 BinLeaf BinLeaf))
                   [4,5]
        ==> [1,2,3,4,5]
    
    Try to find an efficient solution that minimises recopying.
-}    
appendTree :: BinTree a -> [a] -> [a]
appendTree t xs = inorderEfficient t ++ xs

{- provided solution, avoids all concatenations

    Walkthrough to see why it works:
    
    at == appendTree'
    N  == BinNode
    L  == BinLeaf
    
        at tree12 [4,5]
    ==> (N 2 t1 t2) [4,5]                               {match BinNode}
    ==> at t1 (2 : at t2 [4,5])                         {apply at}
    ==> at (N t1' t2') (2: at (N 3 t3 t4) [4,5])        {match BinNode)
    ==> at t1' (1: at t2' ((2:at (N 3 t3 t4) [4,5])))   {apply at}
    ==> at L (1: at t2' ((2:at (N 3 t3 t4) [4,5])))     {match BinLeaf}
    ==> (1 : at t2' ((2:at (N 3 t3 t4) [4,5])))         {apply at}
    ==> (1 : at L ((2:at (N 3 t3 t4) [4,5])))           {match BinLeaf}
    ==> (1 : ((2:at (N 3 t3 t4) [4,5])))                {apply at}
    ==> (1 : (2 : at t3 (3 : at t4 [4,5])))             {apply at}
    ==> (1 : (2 : L (3 : at t4 [4,5])))                 {match BinLeaf}
    ==> (1 : (2 : (3 : at t4 [4,5])))                   (apply at}
    ==> (1 : (2 : (3 : L [4,5])))                       {match BinLeaf}
    ==> (1 : (2 : (3 : [4,5] )))                        {apply at}
    ==> (1 : (2 : [3,4,5]))                             {apply cons}
    ==> (1 : [2,4,4,5])                                 {apply cons}
    ==> [1,2,3,4,5]                                     {apply cons}
    
-}

appendTree' :: BinTree a -> [a] -> [a]
appendTree' BinLeaf ks = ks
appendTree' (BinNode n t1 t2) ks 
    = appendTree' t1 (n : appendTree' t2 ks)

tree12 = BinNode 2 (BinNode 1 BinLeaf BinLeaf)
                   (BinNode 3 BinLeaf BinLeaf)
                   
ex12  = appendTree tree12 [4,5]
ex12a = appendTree' tree12 [4,5]
ex12b = appendTree' tree6 [8,9]                   


     
                                
                            
                            

           
