{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
module W07RMFT where

import Data.Monoid

{-
    Week 07 - Folds and Monoids
              Notes from Suggested Readings for Monoids
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        07-folds-monoids.html
        
    NOTES:
        the article is not really for beginners
        it got more difficult to follow as it progressed;  
        the code, as given, did not always compile nor were
        there any actual tree examples
      
    TODO:      
        There is a FingerTree package that appears to incorporate
        the technique explained in the article; possibly track
        down some tutorials that use it
        
        http://hackage.haskell.org/package/fingertree-0.1.0.1/docs/
                Data-FingerTree.html
-} 
{-
    Ref: Heinrich Apfelmus - Monoids and Finger Trees
         http://apfelmus.nfshost.com/articles/monoid-fingertree.html

    Monoids as 2-3 Finger Trees allow you to fast implementations
    for basically a wide variety of data structure i.e. priority queues,
    sequences, search trees, etc.
    
    The finger trees work with elements that are related to monoids
    with the monoid type chosen varys with the data structure.
    
    To see how this works, start with a simple example: a random access
    list. Normally it takes 0(n) (linear time) to retrieve a random
    element: xs !! n. We'd like an implementation that runs in 
    O(log n).
    
    To begin, we design a Tree structure that holds its elements, a,
    in its leaves and whose every node is annotated with a value, v,
    which is the size of the subtree; so our trees look like:
    
    
                     v               5                                              
                   /   \           /   \                                                                                                        
                  v     v         2     3                                                   
                 / \   / \       / \   / \                                                    
                v  v  v   v     1  1  1   2                                                   
                a  a  a  / \    a  a  a  / \                                                   
                        v   v           1   1                                                    
                        a   a           a   a                                                    
                                    
    Elements in the leaves are stored left to right. The annotations
    (v-values) met the following constraints
    
        tag (Leaf  ..)       = 1
        tag (Branch .. x y)  = tag x + tag y
    
    To make sure the annotations (size values) are always correct
    we use smart constructors: leaf and branch, which ensure the
    tag constraints are met.
    
    The 'tag' function returns an annotation (v-value).
                                    
-}

data Tree v a = Leaf   v a
              | Branch v (Tree v a) (Tree v a)
    deriving (Show)
              
type Size = Int

-- smart constructors
leaf :: a -> Tree Size a
leaf a = Leaf 1 a

branch :: Tree Size a -> Tree Size a -> Tree Size a
branch x y = Branch (tag x + tag y) x y
              
-- return the v-value of the given tree structure              
tag :: Tree v a -> v
tag (Leaf   v _)   = v
tag (Branch v _ _) = v         
              
toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y     

-- return the leftmost element
tHead :: Tree v a -> a
tHead (Leaf   _ a)   = a
tHead (Branch _ x _) = tHead x

-- return the element at the given index
(!!*) :: Tree Size a -> Int -> a
(Leaf   _ a)    !!* 0 = a
(Branch _ x y)  !!* n 
     | n < tag x     = x !!* n
     | otherwise     = y !!* (n - tag x)
(!!*) _ _            = error "invalid index"    
     
tree1 = branch (branch (leaf 'a') (leaf 'b'))
               (branch (leaf 'c') (branch (leaf 'd') (leaf 'e')))    

ex1 = (!!*) tree1 3   -- 'd'
     
{-
    A Priority Queue
    ----------------
    The data structure stores different priorities as integers
    and returns the most urgent (the smallest) first.
    
                             2
                           /   \
                          4     2
                         / \   / \
                        16  4  2  8
                        a   a  a / \
                                32  8
                                a   a    
    
    Note that the tree root has the value of the most urgent priority: 2         
    
    This requires slightly different smart constructors to satisfy
    annotations of 
        tag (Leaf .. a)     = priority a
        tag (Branch .. x y) = tag x `min` tag y
-}                          

type Priority = Int
      
-- smart constructors
leaf' :: Priority -> a -> Tree Priority a
leaf' p a = Leaf p a

branch' :: Tree Priority a -> Tree Priority a -> Tree Priority a
branch' x y = Branch (tag x `min` tag y) x y
      
tree2 = branch' (branch' (leaf' 16 'a') (leaf' 4 'b'))
                (branch' (leaf' 2 'c') 
                         (branch' (leaf' 32 'd') (leaf' 8 'e')))  

-- return node with most urgent priority                         
winner :: Tree Priority a -> a
winner t = go t
    where
    go (Leaf   _ a)        = a
    go (Branch _ x y)
        | tag x == tag t = go x   -- winner on left
        | tag y == tag t = go y   -- winner on right     

ex2 = winner tree2    -- 'c'  

{-
    Monoids - the grand unifier
    ---------------------------
    
    The same tree structure was used for two entirely different
    purposes simply by using different annotations. By recognizing
    that the tags form a 'monoid' we can unify both implementations.
    Furthermore, the (!!*) and winner functions are special cases
    of the same function.
    
    Annotations are monoids
    -----------------------
    
    We can obtain the tags of a branch using the monoid 'mappend' 
    function (for which we use (<>), defined as the mappend operator 
    in Data.Monoid)
        
        tag (Branch _ x y) = tag x <> tag y
        
    by setting up the correct monoid instances and smart constructors.
    
    So the tree structures, with their annotations, become
    
        (v1<>v2) <> (v3<>v4)         v1 <> (v2<>(v3<>v4))
               /    \                  /  \
              /      \               v1   v2 <> (v3<>v4)
             /        \              a1     /  \
         v1 <> v2  v3 <> v4               v2   v3 <> v4 
           /  \      /  \                 a2     /  \
          v1  v2    v3  v4                     v3   v4
          a1  a2    a3  a4                     a3   a4    
          
    Generally, the tag at the root of the tree is
    
        measure a1 <> measure a2 <> measure a3 <> ... <> measure an
        
-}        

-- annotation instance for size of tree
instance Monoid Size where
    mempty  = 0
    mappend = (+)

-- for the leaves, the tag is obtained from the element
class Monoid v => Measured a v where
    measure :: a -> v

instance Measured a Size where
   measure _ = 1                   -- one element = size 1
    
branch'' :: Monoid Size => Tree v a -> Tree v a -> Tree v a
branch'' x y = Branch (tag x <> tag y) x y

leaf'' :: Measured a v => a -> Tree v a
leaf'' a = Leaf (measure a) a

instance Measured a v => Measured (Tree v a) v where
    measure = tag
    
search :: Measured a v => (v -> Bool) -> Tree v a -> Maybe a
search p t
    | p (measure t) = Just (go mempty p t)
    | otherwise     = Nothing
    where
    go i p (Leaf _ a) = a
    go i p (Branch _ l r)
        | p (i <> measure l) = go i p l
        | otherwise          = go (i <> measure l) p r
        

