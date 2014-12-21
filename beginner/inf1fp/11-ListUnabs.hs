-- Informatics 1 - Functional Programming 
-- Lecture 11 - Abstract Types
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect11.pdf
-- Video:  4/11/2014

-- Representing a Set as an unordered lists
-- 
--      Abstraction can be easily broken,
--      nothing to prevent anyone from using any
--      list operation on a set
--
-- Can create two sets that are equal, as sets,
--      [1,2,3] and [3,2,1]
-- but if someone applies the list function, head, the first
-- set will return 1 and the second, 3. So in methematical
-- terms, 'head' is not a function on a set (functions, in math
-- must return the same outputs given the same inputs)
--
module ListUnabs
  (Set,nil,insert,set,element,equal,check) where
import Test.QuickCheck

type Set a = [a]    -- Set is a synonym for a List

nil :: Set a        -- empty Set is an empty List
nil =  []

-- insert a new element (constant time  O(1))
insert :: a -> Set a -> Set a   
insert x xs  =  x:xs

-- convert a list into a Set (constant time O(1))
set :: [a] -> Set a             
set xs  =  xs

-- check if element is in the Set (linear time  O(n))
element :: Eq a => a -> Set a -> Bool  
x `element` xs  =  x `elem` xs

-- define equality between sets
-- A set of 1 and 2 can be represented in multiple ways
--  [1,2], [2,1], [1,2,1], etc.
-- need to account for the different ways
--
-- xs and ys are equal as set if xs is a subset of ys
-- and ys is a subset of xs
-- ie all the elements of xs appear in ys and vice versa
--
-- expensive, 2 times the time it takes to do subset
--            subset is size of xs * size of ys
--            quadratic + length n = n^2 + n
-- (quadratic time O(n^2))
equal :: Eq a => Set a -> Set a -> Bool  
xs `equal` ys  =  xs `subset` ys && ys `subset` xs
  where
  xs `subset` ys  =  and [ x `elem` ys | x <- xs ]

prop_element :: [Int] -> Bool
prop_element ys  =
  and [ x `element` s == odd x | x <- ys ]
  where
  s = set [ x | x <- ys, odd x ]

check =
  quickCheck prop_element
-- Prelude ListUnabs> check
-- +++ OK, passed 100 tests.
