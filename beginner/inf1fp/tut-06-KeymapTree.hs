-- Informatics 1 Functional Programming
-- Tutorial 6 Barcode Reader - Indexed data represented as a tree
--
-- Source:
--  http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/tutorials/tutorial6.zip

{-
    Keymaps as trees
    
    In KeymapTree we will implement the same functions and data type as 
    we had in KeymapList, so from the outside they will look the same. 
    However, internally they will be very different, and so will
    their performance.

    Basically, the data is stored in the nodes of a tree. The left branch
    of a node only stores data that is smaller than the data at the node
    itself, while the right branch stores data that is larger.
    
    When building a keymap in the shape of a tree, we want to make sure 
    that the tree remains sorted. That is, for any node with a certain key,
    the keys in the left subtree should all be smaller than that key, and 
    the keys in the right subtree should all be larger. To ensure this, we
    make sure a user of these keymaps can only access them through 
    functions that are safe.
    
    By not exporting the constructors Node and Leaf themselves, we prevent
    people from writing recursive functions on our trees|at least outside 
    of KeymapTree.hs).
-}

module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT                  
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad        -- needed for QuickCheck routines
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

{- Exercise 6

    (a) Look at the function size. How does it work, and how can we 
        recurse over trees?
        
            recurses over the left and right trees counting a
            value of 1 for each recurse
-}

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

{-
    (b) Define the function depth, which takes a tree and returns the 
        maximal depth of the tree, i.e. the length of the longest path 
        from its root to any of its leaves. A leaf should have depth 0.
-}
-- provided solution
depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + (depth left `max` depth right )

{-
    (c) Load up KeymapTree.hs into GHCi and try the functions size and 
        depth on the little test tree testTree (it should have size 4 and 
        depth 3).
        
            *KeymapTree> size testTree
            4
            (0.02 secs, 18363696 bytes)
            *KeymapTree> depth testTree
            3
            (0.00 secs, 0 bytes)
        
-}

{- Exercise 7
    
    Define the function toList, which takes a tree and returns the 
    corresponding list of pairs. Try it on testTree. Can you make it so 
    that the returned list is sorted?
-}

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf  = []
toList (Node key value left right) 
    = toList left ++ [(key, value)] ++ toList right

{- Exercise 8

    Take a look at the function set. The function defines a helper 
    function f to do the recursion, to avoid repeating the variables
    key and value too often in the definition.
    
    (a) Explain what the function f does when it encounters a leaf.
    
            It creates a new node with the given key and value.
    
    (b) Explain what the function f does when it looks at a node and it 
        encounters the key it was looking for? 
        
            If it finds the given key it chantes the value to the
            given value
            
        Complete the definition of  this function. Hint: the last two 
        cases will need to recurse down an appropriate branch.
        (Note: solution shown is the provided solution)

-}
set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

{- Exercise 9

    Complete the function get. Remember that you should return a 
    Maybe-value. When should it return Nothing, and when should it return 
    Just a value? Test your function on testTree first, and then use 
    QuickCheck to verify prop_set_get.
-}

get :: Ord k => k -> Keymap k a -> Maybe a
get key = f
    where
        f Leaf = Nothing
        f (Node k v left right) 
            | key == k  = Just v
            | key <= k  = f left
            | otherwise = f right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

{- Exercise 10

    Write a function fromList to turn a list into a tree. You should use 
    the function set to add each element to the tree. Think about what the
    tree is that you should start out with. For this question you can use 
    recursion over the input list, but you could also try to use foldr
    and uncurry.
    
    Use the test property prop_toList_fromList to test your solutions. If 
    you managed to return sorted lists with toList, you can also test with
    prop_toList_fromList_sorted.
-}
-- original solution
fromList' :: Ord k => [(k,a)] -> Keymap k a
fromList' ((key, value):lst) = f lst (set key value Leaf)
    where
        f ((k,v): xs ) km = f xs (set k v km)
        f [] km           = km
fromList' _ = Leaf

-- after working out the above
fromList :: Ord k => [(k,a)] -> Keymap k a
fromList = foldr (uncurry set) Leaf

prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

{- Exercise 12
    
    Define two functions filterLT and filterGT, such that filterLT k t is 
    the tree that results from removing all elements in t whose key is 
    greater than k. For example:
    
        *Main> filterLT "0900000000000" testDB
        [("0042400212509",("Universal deep-frying pan","pc"))
        ,("0265090316581",("The Macannihav'nmor Highland Single Malt",
            "75ml bottle"))
        *Main> filterLT "0" testDB
        []

    The function filterGT k t is the tree that results from removing all 
    elements in t whose key is less than k. So:
        *Main> filterGT "0900000000000" testDB
        [("0903900739533",("Bagpipes of Glory","6-CD Box"))
        ,("9780201342758",
        ("Thompson - \"Haskell: The Craft of Functional Programming\"",
            "Book"))]
        *Main> filterGT "0" testDB
-}

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT key = f
    where
        f Leaf = Leaf
        f (Node k v left right) 
            | k == key  = left
            | k < key   = Node k v left (f right)
            | otherwise = f left
                                
filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT key = f
    where
        f Leaf = Leaf
        f (Node k v left right)
            | k == key  = right
            | k < key   = f right
            | otherwise = Node k v (f left) right
            


{- Exercise 13

    Define the function merge, which takes two trees and produces a single
    tree of all their elements. Write a suitable quickCheck test for your
    function.
-}
-- based on provided solution
merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf km = km
merge km Leaf = km
merge t1@(Node k1 v1 l r) t2@(Node k2 v2 l2 r2)
    | k1 == k2  = Node k1 v1 (merge l l2) (merge r r2)
    | otherwise = Node k1 v1 (merge l (filterLT k1 t2))
                             (merge r (filterGT k1 t2))
  
-- provided solution  
prop_merge :: Keymap Int Int -> Keymap Int Int -> Bool                
prop_merge t1 t2 
    = sort (nubBy p (toList t1 ++ toList t2)) == toList (merge t1 t2)
  where p (k,_) (k',_) = k == k'
  
{- Exercise 14

    Define the function del, which takes a key and a tree, and returns a 
    tree identical to its argument except with any entry for the given 
    key deleted. You will need the function merge here.
-}
-- based on provided solution
del :: Ord k => k -> Keymap k a -> Keymap k a
del key = delKey
    where
        delKey Leaf     = Leaf
        delKey (Node k v left right)
            | key == k  = merge left right
            | key <  k  = Node k v (delKey left) right
            | otherwise = Node k v left (delKey right)

{- Exercise 15
    Define the function select, which takes a predicate and a tree, and 
    returns a new tree that contains only entries where the value 
    satisfies the predicate.
-}
-- provided solution
select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select _ Leaf = Leaf
select f (Node k v left right) 
    | f v       = Node k v (select f left) (select f right)
    | otherwise = merge (select f left) (select f right) 

-- Instances for QuickCheck -----------------------------
-- from provided solutions
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
