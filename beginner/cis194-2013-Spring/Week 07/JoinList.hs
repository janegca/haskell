module JoinList where

import Data.Monoid

{-
    Week 07 - Folds and Monoids
              Homework
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/hw/
        07-folds-monoids.pdf
-} 
-- represent append operations as data structures
-- the 'm' parameter will be used to track monodial annotations
-- the annotation at the root will be a combination of all the
-- annotations on the Single nodes
-- Empty nodes are considered to have an annotation of mempty

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)   
    
{-
    Exercise 1
    
    Write an append function for JoinLists that yields a new 
    JoinList whose monoidal annotation is derived from those of 
    the two arguments.
    
         (+++) :: Monoid m => JoinList m a 
                           -> JoinList m a 
                           -> JoinList m a

    You may find it helpful to implement a helper function
    
        tag :: Monoid m => JoinList m a -> m

    which gets the annotation at the root of a JoinList.

-}    
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a Empty = a
(+++) Empty b = b
(+++) a b     = Append (mappend (tag a) (tag b)) a b

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _ ) = m


