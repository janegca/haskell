-- Informatics 1 - Functional Programming 
-- Lecture 11 - Abstract Types
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect11.pdf
-- Video:  4/11/2014
--
-- Set as a balanced  (AVL) tree
{-
    Enforces invariant through insertion

-}
module BalancedTreeUnabs
  (Set(Nil,Node),nil,insert,set,element,equal,check) where
import Test.QuickCheck

-- every Node records the 'depth' which is the length
-- of the maximum path from that point to the leaf
type  Depth  =  Int
data  Set a  =  Nil | Node (Set a) a (Set a) Depth

node :: Set a -> a -> Set a -> Set a
node l x r  =  Node l x r (1 + (depth l `max` depth r))

depth :: Set a -> Int
depth Nil  =  0
depth (Node _ _ _ d) = d

list :: Set a -> [a]
list Nil             =  []
list (Node l x r _)  =  list l ++ [x] ++ list r

-- invariant checks for balance
invariant :: Ord a => Set a -> Bool
invariant Nil   =  True
invariant (Node l x r d)  =
  invariant l && invariant r &&
  and [ y < x | y <- list l ] &&
  and [ y > x | y <- list r ] &&
  abs (depth l - depth r) <= 1 &&       -- check depth of length and right
  d == 1 + (depth l `max` depth r)      -- differs by, at most, 1

nil :: Set a
nil  =  Nil

-- insert with re-balancing 
-- (logarithmic time O(log n))
insert :: Ord a => a -> Set a -> Set a
insert x Nil  =  node nil x nil
insert x (Node l y r _)
  | x == y     =  node l y r
  | x < y      =  rebalance (node (insert x l) y r)
  | x > y      =  rebalance (node l y (insert x r))

-- complexity: O(n log n)
set :: Ord a => [a] -> Set a
set  =  foldr insert nil

-- re-balancing of nodes is done locally; will only
-- take the time of longest path from the current
-- node down
rebalance :: Set a -> Set a
rebalance (Node (Node a x b _) y c _)
  | depth a >= depth b && depth a > depth c
  = node a x (node b y c)
rebalance (Node a x (Node b y c _) _)
  | depth c >= depth b && depth c > depth a
  = node (node a x b) y c
rebalance (Node (Node a x (Node b y c _) _) z d _)
  | depth (node b y c) > depth d
  = node (node a x b) y (node c z d)
rebalance (Node a x (Node (Node b y c _) z d _) _)
  | depth (node b y c) > depth a
  = node (node a x b) y (node c z d)
rebalance a  =  a

-- logarithmic (O(log n)) as tree is always balanced
element :: Ord a => a -> Set a -> Bool
x `element` Nil  =  False
x `element` (Node l y r _)
  | x == y     =  True
  | x < y      =  x `element` l
  | x > y      =  x `element` r

-- linear time  O(n)
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

-- Prelude SetBalancedTreeUnabs> check
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
