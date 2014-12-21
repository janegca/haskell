-- Informatics 1 - Functional Programming 
-- Lecture 11 - Abstract Types
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect11.pdf
-- Video:  10/11/2014 13 to 33 minute mark
--
--  Note that constructor for the 'Set' data type is NOT exported
--  and all functions for working with the type MUST pack and unpack
--  a Set using pattern matching on the constructor
--
-- changes:
--   TreeUnabs     --> TreeAbs
--   Set(Nil,Node) --> Set
-- rest of file identical to TreeUnabs

module TreeAbs
  (Set,nil,insert,set,element,equal,check) where

import Test.QuickCheck

data  Set a  =  Nil | Node (Set a) a (Set a)

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

insert :: Ord a => a -> Set a -> Set a
insert x Nil  =  Node Nil x Nil
insert x (Node l y r)
  | x == y     =  Node l y r
  | x < y      =  Node (insert x l) y r
  | x > y      =  Node l y (insert x r)

set :: Ord a => [a] -> Set a
set  =  foldr insert nil

element :: Ord a => a -> Set a -> Bool
x `element` Nil  =  False
x `element` (Node l y r)
  | x == y     =  True
  | x < y      =  x `element` l
  | x > y      =  x `element` r

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
