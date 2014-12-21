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
module ListAbs
  (Set,nil,insert,set,element,equal,check) where
import Test.QuickCheck

-- supply a constructor
data  Set a  =  MkSet [a]

nil :: Set a
nil =  MkSet []

insert :: a -> Set a -> Set a
insert x (MkSet xs)  =  MkSet (x:xs)

set :: [a] -> Set a
set xs  =  MkSet xs

element :: Eq a => a -> Set a -> Bool
x `element` (MkSet xs)  =  x `elem` xs

equal :: Eq a => Set a -> Set a -> Bool
MkSet xs `equal` MkSet ys  =
  xs `subset` ys && ys `subset` xs
  where
  xs `subset` ys  =  and [ x `elem` ys | x <- xs ]

prop_element :: [Int] -> Bool
prop_element ys  =
  and [ x `element` s == odd x | x <- ys ]
  where
  s = set [ x | x <- ys, odd x ]

check =
  quickCheck prop_element

-- Prelude ListAbs> check
-- +++ OK, passed 100 tests.
