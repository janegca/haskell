{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

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
    
lst, lstA, lstB :: JoinList (Product Integer) Char
lst = Append (Product 210)
        (Append (Product 30)
            (Single (Product 5) 'y')
            (Append (Product 6)
                (Single (Product 2) 'e')
                (Single (Product 3) 'a')))
        (Single (Product 7) 'h')    
        
lstA = (Single (Product 2) 'e')
lstB = (Single (Product 3) 'a')
    
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

ex1a:: JoinList (Product Integer) Char
ex1a = lstA +++ lstB

ex1b :: Integer
ex1b = getProduct $ tag ex1a

{-
    Exercise 2 

    The first annotation to try out is one for fast indexing
    into a JoinList. The idea is to cache the size (number of data elements)
    of each subtree. This can then be used at each step to determine
    if the desired index is in the left or the right branch.
    
    We have provided the Sized module that defines the Size type,
    which is simply a newtype wrapper around an Int. In order to make
    Sizes more accessible, we have also defined the Sized type class
    which provides a method for obtaining a Size from a value.
    
    Use the Sized type class to write the following functions.
    
    1.  Implement the function
            indexJ :: (Sized b, Monoid b) =>
                    Int -> JoinList b a -> Maybe a
                    
        indexJ finds the JoinList element at the specified index. If the
        index is out of bounds, the function returns Nothing    

-}
-- Soln: https://github.com/pdswan/cis194/blob/master/hw7/JoinList.hs
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
    | i == 0    = Just a
    | otherwise = Nothing
indexJ i (Append _ left right)
    | i < leftSize = indexJ i left
    | otherwise    = indexJ (i - leftSize) right
    where leftSize = getSize . size $ tag left

-- test answer    
sjl :: JoinList Size Char
sjl = (Append (Size 4)
            (Append (Size 3)
                (Single (Size 1) 'y')
                (Append (Size 2)
                    (Single (Size 1) 'e')
                    (Single (Size 1) 'a')))
            (Single (Size 1) 'h'))            
            
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)   

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2         

test21 :: (Sized m, Monoid m, Eq a) => Int -> JoinList m a -> Bool
test21 i jl = (indexJ i jl) == (jlToList jl !!? i) 

ex21a, ex21b :: Bool
ex21a = test21 3 sjl
ex21b = test21 1 sjl

sjl1, sjl2, sjl3 :: JoinList Size Char
sjl1 = Single (Size 1) 'm'
sjl2 = Single (Size 1) 'n'
sjl3 = (+++) sjl1 sjl2

{-
    Exercise 2
    
    Implement the function
        dropJ :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> JoinList b a

    The dropJ function drops the first n elements from a JoinList.
    This is analogous to the standard drop function on lists. Formally,
    dropJ should behave in such a way that

        jlToList (dropJ n jl) == drop n (jlToList jl).

-}
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n j | n <= 0       = j
dropJ _ Empty            = Empty
dropJ _ (Single _ _)     = Empty
dropJ n (Append _ t1 t2) 
    | n < s1    = (dropJ n t1) +++ t2
    | otherwise = dropJ (n - s1) t2
    where s1 = getSize . size $ tag t1
    
ex22a, ex22b :: JoinList Size Char    
ex22a = dropJ 2 sjl
ex22b = dropJ 0 sjl

ex23c :: Bool
ex23c = jlToList (dropJ 3 sjl) == drop 3 (jlToList sjl)

{-
    Exercise 2.3

    Finally, implement the function
        takeJ :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> JoinList b a
    
    The takeJ function returns the first n elements of a JoinList,
    dropping all other elements. Again, this function works similarly
    to the standard library take function; that is, it should be the case
    that
        jlToList (takeJ n jl) == take n (jlToList jl).
    
    Ensure that your function definitions use the size function from
    the Sized type class to make smart decisions about how to descend
    into the JoinList tree.
-}    
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0     = Empty 
takeJ _ Empty          = Empty
takeJ _ t@(Single _ _) = t
takeJ n (Append _ t1 t2) 
    | n < s1    = takeJ n t1
    | n == s1   = t1
    | otherwise = t1 +++ takeJ (n-s1) t2
    where s1 = getSize . size $ tag t1

ex23 :: Bool
ex23 = jlToList (takeJ 2 sjl) == take 2 (jlToList sjl)    
    
{-
    Exercise 3
    
    Mr. Dickens’s publishing company has changed their
    minds. Instead of paying him by the word, they have decided to pay
    him according to the scoring metric used by the immensely popular
    game of ScrabbleTM. You must therefore update your editor imple-  
    mentation to count Scrabble scores rather than counting words.
    
    Hence, the second annotation you decide to implement is one
    to cache the Scrabble(TM) score for every line in a buffer. Create a
    Scrabble module that defines a Score type, a Monoid instance for
    Score, and the following functions:    
    
        score :: Char -> Score
        scoreString :: String -> Score

    The score function should implement the tile scoring values as
    shown at http://www.thepixiepit.co.uk/scrabble/rules.html; any
    characters not mentioned (punctuation, spaces, etc.) should be given
    zero points.
    
    To test that you have everything working, add the line import Scrabble
    to the import section of your JoinList module, and write the following
    function to test out JoinLists annotated with scores:
    
        scoreLine :: String -> JoinList Score String
        
    Example:
        *JoinList> scoreLine "yay " +++ scoreLine "haskell!"
        Append (Score 23)
        (Single (Score 9) "yay ")
        (Single (Score 14) "haskell!")    

-}
scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs

ex3 :: JoinList Score String
ex3 = scoreLine "yay " +++ scoreLine "haskell!"

{-
    Exercise 4
    
    Finally, combine these two kinds of annotations. A pair
    of monoids is itself a monoid:
    
        instance (Monoid a, Monoid b) => Monoid (a,b) where
            mempty = (mempty, mempty)
            mappend (a1,b1) (a2,b2) = (mappend a1 a2, mappend b1 b2)
    
    (This instance is defined in Data.Monoid.) This means that join-lists
    can track more than one type of annotation at once, in parallel, simply
    by using a pair type.
    
    Since we want to track both the size and score of a buffer, you
    should provide a Buffer instance for the type    

        JoinList (Score, Size) String 
        
    (Note that you will have to enable the FlexibleInstances and
          TypeSynonymInstances extensions.)        
    
    Due to the use of the Sized type class, this type will continue to work
    with your functions such as indexJ.
    
    Finally, make a main function to run the editor interface using
    your join-list backend in place of the slow String backend (see
    StringBufEditor.hs for an example of how to do this). You should
    create an initial buffer of type JoinList (Score, Size) String and
    pass it as an argument to runEditor editor. Verify that the editor
    demonstration described in the section “Editors and Buffers” does
    not exhibit delays when showing the prompt.
    
    [Note: this is using the Scrabble scoring, not sure that is what
           was intended in the exercise. The StringBufEditor loads
           carol.txt much quicker]
-}        

-- Ref solution: 
--      https://github.com/gsnewmark/cis194/src/Cis194/Week7/JoinList.hs
instance Buffer (JoinList (Score, Size) String) where
    line     = indexJ
    numLines = getSize  . snd   . tag
    value    = getScore . fst   . tag
    toString = unlines  . jlToList
    
    fromString = foldr (+++) Empty 
               . map (\x -> Single (scoreString x, Size 1) x) . lines
                    
    replaceLine n l b = p +++ Single (scoreString l, Size 1) l +++ s
        where p = takeJ n b
              s = dropJ (n + 1) b

tlines :: [String]              
tlines = [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

buf :: JoinList (Score, Size) String         
buf = fromString $ unlines tlines :: JoinList (Score, Size) String

main :: IO ()
main = runEditor editor buf



        

         
         
 