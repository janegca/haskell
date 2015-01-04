{-
    Exercise - Parametric Polymorphism
    Source:
        CIS 194: Introduction to Haskell (Fall 2014), Richard Eisenberg
        Week 4 Homework http://www.seas.upenn.edu/~cis194/lectures.html
        
        The BST.hs file containing Binary Search Tree definitions
        was d/l from:
        http://www.seas.upenn.edu/~cis194/extras/04-poly/BST.hs
        
    Notes:           
        1. Answers to Exercises 1 thru 12 are probably insufficient.
           
           
    Other references for info on total vs partial functions
    
    https://www.haskell.org/haskellwiki/Partial_functions
    https://www.haskell.org/haskellwiki/Avoiding_partial_functions
     
-}
{-
    General Instructions
    
    In these exercises, the type will be given and you have to
    provide an implementation—any implementation—that type checks.
    
    The only rule is that your implementation must be total. That is,
    given any input, your implementations must always run in a finite
    time and return a value. This means that any of the following are
    disallowed:
    
    • Infinite recursion
    • Pattern-matching that does not contain all cases
    • undefined
    • error
    • Any function beginning with unsafe.
    • The use of non-total functions, such as head, tail, init, etc.
    
    This list is not complete, but you will generally write total 
    functions unless you’re trying to be devious. (Or accidentally 
    write a loop.)
    
    After writing your implementation for each exercise, write a comment
    describing the nature of all implementations of functions with
    that type. In particular, if the number of functions that can inhabit
    the type is finite, say what it is. If it’s not finite, try to come 
    up with properties of the functions that you can read right from 
    the type.
    
    Your answers do not need to include explanations.

    For each exercise, you must include at least one interesting fact
    about functions implementing the type. More is better, though!
    
    It is possible that some of the types below have no possible 
    implementations. If that’s the case, make the function’s definition 
    be error "impossible". Your comment must say why the function is
    impossible to write.
    
-}
{-# OPTIONS_GHC -Wall #-}

module HW04 where

import Data.Maybe
import Data.Char (isUpper)
import Data.List (intercalate)
import BST

{- Ex 1
    Takes two arguments of either the same or different types
    and returns a value of the second type.

    Example:
        *HW04> ex1 2 3
        3
        *HW04> ex1 2 "me"
        "me"
-}
ex1 :: a -> b -> b
ex1 _ b = b

{-  Ex 2 thru 7
    The example type signatures
        
        ex2 :: a -> a -> a
        ex3 :: Int -> a -> a
        ex4 :: Bool -> a -> a -> a
        ex5 :: Bool -> Bool
        ex6 :: (a -> a) -> a
        ex7 :: (a -> a) -> a -> a
        ex8 :: [a] -> [a]
        ex9 :: (a -> b) -> [a] -> [b]
        
    are all impossible to define as 'total functions' as the type
    of the first arg matched to an 'a' becomes the 'default' type
    for the function, any further 'a' args must be of the same type;
    if not, an error will be thrown at runtime.
    
    Exercises 8 and 9 are impossible as they can't return an 
    empty list [a] and/or [b] is NOT [] 
    
    ex10 :: Maybe a -> a
        impossible, 'Nothing' doesn't take a parameter so
        can't return something of the same type
        
-}
{-
    Ex 11
        You can always return any type as 'Just' itself
-}
ex11 :: a -> Maybe a
ex11 a = Just a

{-
    Ex 12 
        You can always create a Maybe from a Maybe
-}
ex12 :: Maybe a -> Maybe a
ex12 (Just a) = Just a
ex12 Nothing  = Nothing

{-
    Ex 13
    Write the insertion method for a Binary Search Tree, defined
    in BST.hs, as:
    
        data BST a = Leaf | Node (BST a) a (BST a)
    
    Recall the definition of Ordering (defined in Prelude)
    
        data Ordering = LT | EQ | GT
        
    This function is actually quite simple—thinking through the answers
    to the following questions will essentially write the function
    for you:
    
    1. What should you do when inserting into an empty tree (that is, a
       Leaf)?
    2. What should you do when inserting x into a non-empty tree
       whose root node has a value greater than x?
    3. What should you do when inserting x into a non-empty tree
       whose root node has a value less than x?
       
    It is interesting to note that, because of parametric polymorphism,
    every call of insertBST must be accompanied by a comparison operation.
    Otherwise, there’s no way to know how to compare elements.
    We’ll see a mechanism—called type classes—that will make this less
    burdensome.
-}
  
insertBST :: (a -> a -> Ordering) -- comparison function
          -> Maybe a               -- new element
          -> BST a                 -- existing tree
          -> BST a                 -- new tree
insertBST _ Nothing t = t         
insertBST _ a Leaf    = Node Leaf (fromJust a) Leaf
insertBST f a (Node l x r) 
    | (f x (fromJust a)) == GT = Node (insertBST f a l) x r
    | otherwise                = Node l x (insertBST f a r)
        
cmpInts :: Int -> Int -> Ordering
cmpInts x y | x < y     = LT
            | otherwise = GT        
            
{-
    Example Usage:
    
        *HW04> insertBST cmpInts (Just 1) Leaf
        Node Leaf 1 Leaf
        *HW04> insertBST cmpInts (Just 3) it
        Node Leaf 1 (Node Leaf 3 Leaf)
        *HW04> insertBST cmpInts (Just 0) it
        Node (Node Leaf 0 Leaf) 1 (Node Leaf 3 Leaf)
        *HW04> isBST cmpInts it
        True
-}            
{-
    Visiting the library
    
    An effective Haskell programmer must know how to use the standard
    libraries. The exercises below will require you to read through
    the documentation of Data.List, Data.Maybe, and Data.Char to write
    succinct solutions. Each of these functions has a simple, one-liner
    answer!    

    Many functions (especially in Data.List) have Eq a => in the
    types. We’ll see exactly what this means shortly. For now, we’ll 
    just say that those functions can only be called on concrete types, 
    like Int or String, but not unknown types, like a.
    
    In our quest to avoid partial functions, you may want to use these:
-} 
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

{-
    Ex 14
    Check to see if a list of strings contains only capitalized
    words:
        allCaps :: [String] -> Bool
    
    Examples:
        allCaps ["Hi","There"] == True
        allCaps []             == True
        allCaps ["", "Blah"]   == False
        allCaps ["Hi","there"] == False
      
    Implementation Notes:
        The body of allCaps is written in 'point free' style
        and is equivalent to:
        
        allCaps xs = all isUpper (map (fromMaybe 'a') (map safeHead xs))
        
        The '.' operator combines functions, allowing the removal
        of parentheses, making for cleaner code
        
        The 'xs' argument is implied 
        
        The function composition (.) operator is  right-associative so
        the code is executed in stages from right to left as follows:
            
             stage 1 map safeHead xs               -> result_1
             stage 2 map (fromMaybe 'a') result_1  -> result_2
             stage 3 all isUpper result_2          -> final_result
             
        Example results for ["", "Blah"]
          map safeHead ["", "Blah"]               -> [Nothing, Just 'B']
          map (fromMaybe 'a') [Nothing, Just 'B'] -> ['a', 'B']
          all isUpper ['a', 'B']                  -> False
            
        Note that 'fromMaybe' takes a default value ('a' in this case)
        to substitute for a 'Nothing' element in a list of Maybe's
        
-}  
allCaps :: [String] -> Bool
allCaps = all isUpper . map (fromMaybe 'a') . map safeHead

{-
    Ex 15
    Drop the trailing whitespace from a string:
        dropTrailingWhitespace :: String -> String

    Examples:
        dropTrailingWhitespace "foo" == "foo"
        dropTrailingWhitespace "" == ""
        dropTrailingWhitespace "bar " == "bar"

-}
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = takeWhile (/= ' ')

{-
    Ex 16
    Get the first letter (if it exists) of a list of strings:
        firstLetters :: [String] -> [Char]

    Examples:
    firstLetters ["foo", "bar"] == [’f’,’b’]
    firstLetters ["alpha",""] == [’a’]
    firstLetters [] == []
    firstLetters ["",""] == []
    
    Implementation Note:
        'mapMaybe' discards all elements in a list of Maybe's that
        match 'Nothing' and extracts the value from each 'Just a'
        element.
-}
firstLetters :: [String] -> [Char]
firstLetters = mapMaybe safeHead

{-
    Ex 17
    Render a proper bracketed list given a list of strings:
        asList :: [String] -> String

    Examples:
        asList ["alpha","beta","gamma"] == "[alpha,beta,gamma]"
        asList [] == "[]"
        asList ["lonely"] == "[lonely]"

-}
asList :: [String] -> String
asList xs = "[" ++ intercalate "," xs ++ "]"

