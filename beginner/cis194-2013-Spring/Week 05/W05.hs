{-# LANGUAGE FlexibleInstances #-}
module W05 where

{-
    Week 05 - More polymorphism and type classes
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        05-type-classes.html
-}
{-
    Parametricity
    -------------
    Haskell uses 'parametric polymorphsm' which means polymorphic functions
        (a) must work uniformly for any input type
        (b) the calling function gets to choose the type
        (c) type checking happens at compile time, NO type information
            is available at runtime
        (d) the type signature determines the behaviours a function
            could possibly have
    
    Examples of Prelude functions having the following type
    signatures:
    
    a -> a                              id
    a -> b                              none
    a -> b -> a                         const
    [a] -> [a]                          cycle, init, reverse, tail
    (b -> c) -> (a -> b) -> (a -> c)    (.)
    (a -> a) -> a -> a                  none
    
    Parametricity corresponds to 'guarantees'. When it is necessary
    that we do something that depends on types we use 'constraints'.
    For example, the type signature for (+) in the Prelude is:
    
        (+) :: Num a => a -> a -> a
        
    Here, the => symbol indicates that the 'a' type variable is
    'constrained' to values belonging to the class Num. (+) is
    said to be 'type class polymorphic'.
-}
{-
    Type Classes
    ------------
    
    The Eq class is defined as:
    
        class Eq a where
            (==) :: a -> a -> Bool
            (/=) :: a -> a -> Bool
            
    It has a single parameter, a, and two functions (methods)
    (==) and (/=). Any type that wants to be an 'instance' of
    Eq must provide definitions for the two methods [similar
    to Interface in Java]
    
    The Eq class has default implementations for both methods
    
        class Eq a where
          (==), (/=) :: a -> a -> Bool
          x == y = not (x /= y)
          x /= y = not (x == y)    

    our own classes can use or override these.
    
    If we want Haskell to automatically generate the methods for
    us we can have our class 'derive' from Eq (or other classes)
    (see Foo' below).
    
    The standard type classes are: Eq, Ord, Num, Show, Read, Integral
-}
data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

  foo1 /= foo2 = not (foo1 == foo2)
  
data Foo' = F' Int | G' Char
    deriving (Eq, Ord, Show)

-- Example of a custom type class
class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  toList True  = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

-- to compute sumL, first convert to a list of Ints, then sum
sumL :: Listable a => a -> Int
sumL x = sum (toList x)

-- we can use multiple type constraints
foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y
 
-- we can add type class constraints to instances 
-- NOTE: toList x ++ toList y IS NOT RECURSIVE
--       the 'toList' calls are on the 'a' and 'b' types
--       not the 'toList' that is being defined
instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y  