-- Informatics 1 - Functional Programming 
-- Lecture 11 - Abstract Types
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect11.pdf
-- Video:  4/11/2014

-- A Set as an ordered list
-- 
-- Breaking abstraction:
--      the 'head' problem from ListUnabs has gone away
--      equality between a set and list can return True
--          even thought one is not really a set
--      testing for members of a list read as a set can
--          return a wrong answer
--
module OrderedListUnabs
  (Set,nil,insert,set,element,equal,check) where

import Data.List(nub,sort)
import Test.QuickCheck

type Set a = [a]        -- Set as synonym for a list

-- checks that the set is in ascending order, as required
invariant :: Ord a => Set a -> Bool
invariant xs  =
  and [ x < y | (x,y) <- zip xs (tail xs) ]

nil :: Set a
nil =  []

-- insert new element in ascending order
-- (linear time O(n))
insert :: Ord a => a -> Set a -> Set a
insert x []               =  [x]
insert x (y:ys) | x < y   =  x : y : ys
                | x == y  =  y : ys
                | x > y   =  y : insert x ys

-- convert a list to a set
-- (the sorting n log n, removing dups is linear)
-- (O n log n)
set :: Ord a => [a] -> Set a
set xs  =  nub (sort xs)

-- ordered search for element
-- (linear time  O(n))
element :: Ord a => a -> Set a -> Bool
x `element` []                =  False
x `element` (y:ys) | x < y    =  False
                   | x == y   =  True
                   | x > y    =  x `element` ys

-- equality has been simplified, just need to 
-- compare lists (linear time O(n))                   
equal :: Eq a => Set a -> Set a -> Bool
xs `equal` ys  =  xs == ys

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

-- Prelude OrderedListUnabs> check
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
