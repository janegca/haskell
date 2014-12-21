-- Informatics 1 - Functional Programming 
-- Lecture 11 - Abstract Types
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect11.pdf
-- Video:  4/11/2014
--
-- Set as a Tree
{-
    Example of an ordered binary tree:
    
                3       Root      Everything to the left is less than
              /   \               the Root; to the right, greater than
             1     4    Nodes
            / \   / \
         Nil Nil Nil Nil
 
    Searches will go down the tree, either on the left or the right
    
    A more complex example
    
                                3               Root
                              /   \
                             2     6            Nodes
                            /  \  /  \ 
                           1    N 4    18       Nodes
                          / \    /  \ /  \
                         N  N   N   N N  N
                         
    The Left subtree would be represented (using Set type defined below)
    as: 
            Node (Node Nil 1 Nil) 2      Nil
            Node  Left            Label  Right
            
    Note: 
        the actual shape of the tree depends on the order in which
        elements are inserted. For example, the first tree, with 
        elements 1,3,4, could have been created by inserting the
        element in the order 1, 3,4  to give the following tree:
        
                1           This is called an 'unbalanced tree'
               /  \         while the trees shown above are
               N   3        'balanced tree'
                  / \
                  N  4      balanced trees minimize search time
                    / \     by shortening the longest path through
                    N  N    the tree
                    
    Breaking abstraction
    
    The abstraction can be broken as the invariant property
    based on the ordering of elements is not enforced.  We could
    build trees without using the defined operations.
-}

module TreeUnabs
  (Set(Nil,Node),nil,insert,set,element,equal,check) where
import Test.QuickCheck

-- set represented as an Algebraic Data type
data  Set a  =  Nil | Node (Set a) a (Set a)

-- convert Tree to List (in order traversal)
-- first traverse the left subtree, then put the label
-- and last traverse the right subtree
-- (linear time)
list :: Set a -> [a]
list Nil           =  []
list (Node l x r)  =  list l ++ [x] ++ list r

invariant :: Ord a => Set a -> Bool
invariant Nil   =  True
invariant (Node l x r)  =
  invariant l && invariant r &&
  and [ y < x | y <- list l ] &&
  and [ y > x | y <- list r ]

nil :: Set a
nil  =  Nil

-- insert a node
-- (complexity (time) is the length of the longest path in the tree
--  worst case is linear time, best case is log n)
-- (avg O(log n), worst O(n))
insert :: Ord a => a -> Set a -> Set a
insert x Nil  =  Node Nil x Nil
insert x (Node l y r)
  | x == y     =  Node l y r
  | x < y      =  Node (insert x l) y r
  | x > y      =  Node l y (insert x r)

-- convert List to a Set
-- (avg, O(n log n), worst, quadratic time O(n^2))
set :: Ord a => [a] -> Set a
set  =  foldr insert nil

-- check for an element
-- (complexity same as insert)
element :: Ord a => a -> Set a -> Bool
x `element` Nil  =  False
x `element` (Node l y r)
  | x == y     =  True
  | x < y      =  x `element` l
  | x > y      =  x `element` r

-- converts the Set (Tree) to a List and then compare  
-- (linear time O(n))
equal :: Ord a => Set a -> Set a -> Bool
s `equal` t  =  list s == list t

prop_invariant :: [Int] -> Bool
prop_invariant xs  =  invariant s
  where
  s = set xs

prop_element :: [Int] -> Bool
prop_element ys  =
  and [ x `element` s == odd x | x <- ys ]
  where
  s = set [ x | x <- ys, odd x ]

check =
  quickCheck prop_invariant >>
  quickCheck prop_element

-- Prelude TreeUnabs> check
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
