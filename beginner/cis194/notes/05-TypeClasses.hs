{-
    CIS194 - Week 5 Type Classes
    Source: http://www.seas.upenn.edu/~cis194/lectures/05-type-classes.html
    
    Suggested Reading:
    http://www.cis.upenn.edu/~cis194/spring13/lectures/04-higher-order.html
        
    Other resources on types and classes:
    Haskell 2010 Report
    https://www.haskell.org/onlinereport/haskell2010/haskellch4.html
    
    Haskell WikiBook
    https://en.wikibooks.org/wiki/Haskell/Classes_and_types
    
    Topics
        Language Pragmas
        Functions
            Functional combinators
            Point-free programming
            Lambda
            Operator sections
            Currying and Partial Application
            Folds
            Functional examples
        Type classes
            Type Classes and Java Interfaces
            Standard Type Classes
        
    
-}
{-# LANGUAGE FlexibleInstances #-}          -- language pragma

import Data.Char  ( isUpper, toUpper )
import Data.Maybe ( mapMaybe )
import Text.Read  ( readMaybe )

{-
    FUNCTIONAL COMBINATORS
    
    Haskell has two functional operators: (.) and ($) defined as
    follows in Prelude:
    
        (.) :: (b -> c) -> (a -> b) -> a -> c
        (.) f g x = f (g x)
        
        ($) :: (a -> b) -> a -> b
        f $ x = f x
        
    The (.) operator combines functions immeadiately
    The ($) makes function execution right vs left associative
    They allow us to get rid of parentheses
-}

-- for each list element, add 1 and multiply by 4
add1Mul4 :: [Int] -> [Int]
add1Mul4 x = map ((*4) . (+1)) x

-- negate all even numbers in a list
negateNumEvens :: [Int] -> Int
negateNumEvens x = negate (length (filter even x))

negateNumEvens' :: [Int] -> Int
negateNumEvens' x = negate $ length $ filter even x

{-
    POINT FREE PROGRAMMING
    
    Both the above examples apply a sequence of operators to a 
    parameter; the parameter itself isn't important, so we can leave 
    it out.
    
    This is where the (.) comes in; it’s the way to combine functions 
    when you want them to perform one after another 
    
    [Note: Concept is similar to piping\
    
-}
add1Mul4' :: [Int] -> [Int]
add1Mul4' = map ((*4) . (+1))

negateNumEvens'' :: [Int] -> Int
negateNumEvens'' = negate . length . filter even

{-
    LAMBDA (Anonymous Functions or Lambda Abstractions)
    
    Used to create anonymous (unnamed) functions; usually to perform
    a one off operation that will not be required in other functions.
    
    Best used for small pieces of code.
    
    In the example below,
        \       is read as 'lambda', it binds the variable
        x       to the expression after the
        ->      x ++ x
        
    Lambda abstractions can also have multiple arguments 
        (\x y z -> x, 2*y, 3*z) 5 6 3 -> [5, 12, 9]
-}
-- duplicating a string
duplicate :: [String] -> [String]
duplicate = map dup
  where dup x = x ++ x
  
-- rather than clutter up the namespace with 'dup', make it an
-- anonymous function
duplicate' :: [String] -> [String]
duplicate' = map (\x -> x ++ x)  

-- extract integers greater than 100 for a list of integers
greaterThan100 :: [Int] -> [Int]
greaterThan100 = filter (\x -> x > 100)

{-  
    OPERATOR SECTIONS
    
    Operator Sections take the form
        (op value) or (value op)
        
    and are equivalent to the lambdas 
        (\var -> var op value) or (\var -> value op var)
        
    e.g. (> 100)  ==> (\x -> x > 100)
         (100 >)  ==> (\x -> 100 > x)
    
    Where possible, you should prefer operator sections to 
    lambdas.  
    
-}
-- although that is better written as using a section
greaterThan100' :: [Int] -> [Int]
greaterThan100' = filter (> 100) 

{-  
    CURRYING AND PARTIAL APPLICATION
    
    "All functions in Haskell take only ONE argument"
    
    For example, a function with a type signature of
    
        f :: Int -> Int -> Int
        
    Does NOT take two arguments of type Int and ouput a third
    It takes one argument of type Int and outputs a FUNCTION
    of type (Int -> Int) that takes one argument of type Int
    and outputs an Int; so the signature is read as:
    
        f :: Int -> (Int -> Int)
        
    The arrows ( -> ) associate to the right
    
        W -> X -> Y -> Z  ==> W -> (X -> (Y -> Z))
    
    On the other hand, FUNCTION APPLICATION IS LEFT-ASSOCIATIVE
    ie  f 3 2 ==> (f 3) 2
    
    Which makes sense if functions only take one argument, returns
    a function that takes the next argument, and so on; so,
    
        f x y z = \x -> (\y -> (\z -> ...))
        
    This idea of writing multi-argument functions as one argument
    functions that return functions is known as CURRYING.
    
    To represent a function that does take two arguments, we
    would write the signature as:
    
        f :: (Int, Int) -> Int
        f (x,y) = 2*x + y
        
    The Prelude has two functions for dealing with functions
    with two arguments:
    
        curry :: ((a,b) ->) -> a -> b -> c
        curry f x y = f (x,y)
        
        uncurry :: (a -> b -> c) -> (a,b) -> c
        uncurry f (x,y) = f x y
        
    'uncurry' comes in handy when you want to apply a function
    to a pair:
        uncurry (+) (2,3)
        
-}
{-
    PARTIAL APPLICATION
    
    Currying makes partial application easy. An opertor section is
    an example, ie (100 <) allows us to 'fix' one of the arguments
    to the '<' function; in effect, we 'partially apply <'.
    
    Every funtion can be partially applied to one argument, the first
    one (except for built-in infix operators).
    
    There is an art to writing function signatures to allow full
    advantage of partial applications; for example, you might
    want to list the least variable of your arguments first and
    those most likely to change, last.
    
-}
{-
    Example of putting all this together
-}

-- initial function
foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs
  
-- re-thinking the function in terms of incremental transformations
-- of the data (the Integer list)
--      1. take each element > 3
--      2. multiply by 7 and add 2
--      3. sum the result
foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (> 3)

{-
    FOLDS
    
    There are a number of processes that reduce a list to single
    value, for example: sum, length, product. What these all have
    in common is the application of some function to an element
    in combination with the same application to the rest of the
    list. For example, sum adds all elements in a list, product
    multiplies and sums all elements in a list and length counts
    all elements in a list.
    
    Prelude provides a 'foldr' function that takes a starting
    value, a function, and then applies the function to reduce
    or 'fold' the list from the right (natural recursion) and
    a 'foldl' function that folds from the left.
    
    i.e.    foldr f z [a,b,c] = a `f` (b `f` (c `f` z))
            foldl f z [a,b,c] = ((z `f` a) `f` b) `f` c
    
    Note that you should use foldl' rather than foldl as it
    uses an accumulator and is more efficient (executes in
    constant memory space).
    
    Examples of sum, product and length implemented explicitly as 
    recursive function and implicitly recursive as folds:
-}
sum' :: [Int] -> Int
sum' []     = 0                 -- zero value => 0 (identity for addition)
sum' (x:xs) = x + sum' xs       -- operation  => (+)

sum'' :: [Int] -> Int
sum'' = foldr (+) 0             -- implicit recursion

product' :: [Int] -> Int
product' []     = 1                 -- zero value => 1  (identity for mult)
product' (x:xs) = x * product' xs   -- operation  => (*)

product'' :: [Int] -> Int
product'' = foldr (*) 1             -- implicit recursion

length' :: [a] -> Int
length' []     = 0                  -- zero value => 0
length' (_:xs) = 1 + length' xs     -- operation  => (+1)

length'' :: [a] -> Int
length'' = foldr (\_ -> (+1)) 0     -- implicit recursion

{-
    FUNCTIONAL EXAMPLES
    
    Following are examples of functional coding.
    
    Note that while it may appear the implementation will build
    and discard large lists, the GHC knows how to optimize these
    idiom forms and the internal lists never get built!
    
-}

-- count uppercase letters in a list of strings
numUppercase :: [String] -> Int
numUppercase = length . filter isUpper . concat

-- convert all letters in a list of strings to upper case
listToUpper :: [String] -> [String]
listToUpper = map (map toUpper)

-- find all digits in a string and add them together
sumStringDigits :: String -> Int
sumStringDigits = sum . mapMaybe read_digit
  where read_digit :: Char -> Maybe Int
        read_digit = readMaybe . listify

        listify :: a -> [a]
        listify x = [x]          -- can also be written as (:[]) x
 
{-
    TYPE CLASSES
    
    So far we've been looking at 'parametric (universal) polymorphism'
    which says a function must work for 'all types' (a). But sometimes
    we want functions to work on a subset of types. For example, 
    we want to use (+) on Numbers but not on type Maybe Char. 
    This second, more specific type of polymorphism, is called
    'ad-hoc polymorphism'; it allows for multiple types but NOT ANY
    type.  Haskell implements 'ad-hoc polymorphims' using 
    'type classes'.
    
    Type classes define a set of operations. We create 'instance
    classes' for the types we want to support those operations.
    i.e. "type classes correspond to 'sets of types' which have certain 
    operations defined for them"
    
    [Note: somewhat similar to Java interfaces; the operation (method)
           type signatures are defined in the class declaration but
           not implemented. 'Classes' are not objects, they are sets
           of operations (behaviours) which various 'types' can support by 
           implementing the operations (behaviours) via an 'instance'
           definition.
           
           In Haskell, an 'instance' of a class is NOT a specific
           class object but a single defined implementation of
           the 'class' operations (behaviours).
           i.e. a 'class' is used to define the interface for a 
               'set of types' (hence, 'type class'). Types can belong
               to a variety of classes; there must be a corresponding
               'instance' definition of for each supported 'class' in
               which all the operations of the class are defined 
               (implemented).
               
    From 'Haskell Wiki-book': "Classes are not types, but categories of 
    types; and so the instances of a class are not values, but types."
                                
    Consider, as an example, the Eq class

        class Eq a where
          (==) :: a -> a -> Bool
          (/=) :: a -> a -> Bool
          
    This can be read as:
        Eq is a declared class with a single (type) parameter 'a'
        Eq has two defined operations (methods): (==) and (/=)
        Any 'type' which wants to an instance of Eq must provide
        an 'instance' of Eq which implements both methods
        
    The actual 'type' of the methods are:
        (==) :: Eq a => a -> Bool
        (/=) :: Eq a => a -> Bool
        
    where the 'Eq => a' acts as a 'constraint' i.e. only types
    belonging to the Eq class can use the functions (==) and (/=).
                
    A type class CAN provide default implementations for some or
    all of their methods; the Eq class in Prelude is actually
    declared as:
    
        class Eq a where
            (==), (/=) :: a -> a -> Bool
            x == y = not (x /= y)
            x /= y = not (x == y)

    So if we want to declare a type as belonging to the class Eq
    we only need to define (==) or (/=) and the other will be 
    automatically defined in terms of the other.
    
    For example, if we have a type 'Foo' that we want to belong to 
    the class Eq we define an instance of Eq Foo as follows:
    
        data Foo = F Int | G Char

        instance Eq Foo where
          (F i1) == (F i2) = i1 == i2
          (G c1) == (G c2) = c1 == c2
          _ == _ = False
    
    [Note: that 'Foo' is passed to Eq through the type variable 'a'
           Remember our definition: class Eq a ... so the class
           definition acts like a constructor for 'instances' of 
           the class but in Haskell the 'instances' are of many
           different 'types' rather than many different objects.
           Somewhat similar to Java method overloading although
           it's probably better to forget Java/C, whatever and
           just try to think in Haskell.]
    
    In reality, the GHC compiler will automatically generate
    'instance' definitions for a few built-in type classes
    (Eq, Ord, Enum, Ix, Bounded, Show, and Read). We could simply 
    have declared our new type as:
    
        data Foo' = F' Int | G' Char
            deriving (Eq, Ord, Show)
    
    The 'deriving' keywords signals that the new type supports
    (has a defined instance of) the Eq, Ord and Show classes.
-}    
{-
    Example User Defined Type Classes
-}
-- a class of things that can be converted to a list of Int's
-- the type of toList is: toList :: Listable a => a -> [Int]  
class Listable a where              
  toList :: a -> [Int]
  
-- types that support the Listable class require 'instance'
-- definitions which implement toList by defining 'how' the
-- type can be converted to a list of Int's
instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]
  
instance Listable Bool where
  toList True  = [1]
  toList False = [0]
  
instance Listable [Int] where
  toList = id                       -- the identity function

-- here is another user defined type that supports Listable
-- the instance implementation describes how the Tree can
-- be flattened to a list of Int's
data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node x l r) = toList l ++ [x] ++ toList r
  
-- when we implement other functions in terms of toList, the
-- Listable a => constraint becomes part of their type signature
sumL x = sum (toList x)

{-
    The type of sumL, when checked in ghci is:
        *Main> :t sumL
        sumL :: Listable a => a -> Int  

    Similarly, the type of
        foo x y = sum (toList x) == sum (toList y) || x < y
        
    is
        foo :: (Listable a, Ord a) => a -> a -> Bool
        
    since both Listable and Ord methods are used by the foo
    function i.e. it will only work on types that belong to
    both the Eq and Ord type classes
    
    As a final, and more complex, example, consider this instance:

    instance (Listable a, Listable b) => Listable (a,b) where
      toList (x,y) = toList x ++ toList y

    Notice how we can put type class constraints on an instance as well 
    as on a function type. This says that a pair type (a,b) is an 
    instance of Listable as long as a and b both are. Then we get to 
    use toList on values of types a and b in our definition of toList 
    for a pair. Note that this definition is not recursive! The version 
    of toList that we are defining is calling other versions of toList, 
    not itself. 

    [Note:  presumably they are calling the toList methods associated 
            with the 'a' and 'b' types, whatever they are]
-}
