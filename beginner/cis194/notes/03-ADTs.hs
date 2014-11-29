-- CIS194 - Week 3 - Algebraic Data Types
-- Source: http://www.seas.upenn.edu/~cis194/lectures/03-ADTs.html
--
-- Suggested Readings:
--      'Real World Haskell' Chapters 2 and 4
--
-- Topics Covered:
--      Libraries
--      Enumeration Types
--      Algebraic Data Types
--      Pattern Matching
--      Case Expressions
--      First Class Functions
--      Recursive Data Type
--
import Data.Char (toUpper)

{-
    LIBRARIES   
        Distributed as packages containing one or more modules
        The base package contains the Prelude module
        New packages can be d/l and installed directly from the
        haskell server using 'cabal'
        
        Example     > cabal update
                    > cabal install text-1.1.1.3
                    
                    'text' is the package name, 
                    '1.1.1.3' is the version number (optional)
                    
        The text package provides a way to store chunks of text
        more efficiently than String; installing v1.1.1.3 is
        recommended as the latest version, 1.2.0.0, may conflict
        with other packages.
-}  

{-
    ENUMERATION TYPES
    
    Haskell allows 'enumeration' types like the data 'Thing' below;
    it has 5 ctors: Shoe, Ship, etc. which are the only 'values' of
    type Thing (note that 'Thing' itself is abstract)
    
-}

data Thing = Shoe 
           | Ship 
           | SealingWax 
           | Cabbage 
           | King
  deriving Show
  
shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]  

isSmall :: Thing -> Bool
isSmall Ship    = False
isSmall King    = False
isSmall _       = True

{-
    ALGEBRAIC DATA TYPES
    
    Enumeration types are just a special case of Haskell's more generic
    'algebraic data type'
    
    Consider FailableDouble type below:
        it has 2 ctors: Failure and OK
        Failure takes no parameters and has the type FailableDouble
        OK takes one parameter, a Double, so OK is NOT a FailableDouble
            on its own, it has type:
                Double -> FailableDouble
                
    Data ctors can have multiple arguments (see Person below)
    Note that with Person, the type and the ctor have the same name
    but each continue to inhabit different name spaces and are
    different things i.e. the is a Person type and a Person ctor
    
    General Form of an Algebraic Data Type
    
        data AlgDataType = Constr1 Type11 Type12
                         | Constr2 Type21
                         | Constr3 Type31 Type32 Type33
                         | Constr4
                         
    This signifies that AlgDataType can be constructed in one of four ways.
    
    Types and data ctors MUST ALWAYS being with a capital letter.
-}
data FailableDouble = Failure       -- :t Failure :: FailableDouble
                    | OK Double     -- :t OK :: Double -> FailableDouble
    deriving Show
                    
ex01 = Failure
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)   = d

-- store a person's name, age, and favourite thing
data Person = Person String Int Thing
    deriving Show
    
richard :: Person
richard = Person "Richard" 32 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _ ) = a

{-
    PATTERN MATCHING
    
    Fundamentally, pattern matching is about taking apart a value
    by finding out which ctor it was built with; this information
    is the basis for deciding what to do.
    
    Pattern matching on our generic type would look like:
        foo (Constr1 a b)   = ...
        foo (Constr2 a)     = ...
        foo (Constr3 a b c) = ...
        foo (Constr4        = ...
        
    Note that we have to give the constructor values names AND
    that constructors with parameters must be wrapped in parentheses
    
    Other things of note in pattern matching:
    
    1. An underscore _ acts as a wild card
    2. A pattern of the form 'x@pat' can be used to match against
       the pattern 'pat' but will also give the name 'x' to the entire
       value being matched. i.e. all@(x:xs) (see 'bax' below)
    3. Patterns can be nested
    
    In general the following describes the grammar of what can be 
    used as a pattern (in general, there is much more to it)
    
        pat ::= _
             |  var
             |  var @ ( pat )
             |  ( Constructor pat1 pat2 ... patn )

-}
    
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- 'SealingWax' is a pattern nested inside 'Person' pattern
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person."
checkFav (Person n _ _)          = n ++ ", your favourite thing is lame."

expm :: String -> String
expm msg@(x:y:xs) = "full txt " ++ msg
expm _ = "Failed"

{-
    CASE EXPRESSIONS
    
    The baisic construct for pattern matching is the 'case' expression
        case exp of
            pat1 -> exp1
            pat2 -> exp2
            ...
               
-}
ex03 = case "Hello" of
           []      -> 3
           ('H':s) -> length s
           _       -> 7
           
-- earlier definition is syntactic sugar for this definition           
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d

ex03a = failureToZero Failure
ex03b = failureToZero (OK 3)
ex03c = failureToZero' Failure
ex03d = failureToZero' (OK 3)

{-
    FIRST CLASS FUNCTIONS
    
    Functions are treated as 'chunks of data', just like any other
    chunk of data
    Just as the type of a character is written with 'Char', the type
    of a function is written with '->'. The difference being that a 
    function type must declare the specify the DOMAIN of the function
    (the type of arguments it takes) and the CODOMAIN (type of the
    result it produces)
    
    A function's RANGE is a subset of the function's CODOMAIN
    i.e. a RANGE is the set of values a function ACTUALLY produces
         a CODOMAIN is the set of values a function could POTENTIALLY
            produce
    
    One of the most important functions in a functional programming
    language is 'map'. Here we'll just talk about a 'map' version
    that works on integers.
    
    Functions that take other functions as arguments are called
    HIGHER ORDER FUNCTIONS
-}                  

-- mapInteger takes a function 'f' (Integer -> Integer)
-- and (x:Xs), a list of integers [Integers], applies the function to
-- each element in the list (f x) returning the results as a
-- list of integers [Integers]
--
mapInteger :: (Integer -> Integer) -> [Integer] -> [Integer]
mapInteger _ []     = []
mapInteger f (x:xs) = f x : mapInteger f xs

-- a fn that takes and returns an Integer
plus1 :: Integer -> Integer
plus1 x = x + 1

-- a function that applies an element function, plus1, to a list
-- using mapInteger
twoThreeFour :: [Integer]
twoThreeFour = mapInteger plus1 [1,2,3]

-- the Prelude 'map' function is polymorphic, can work with any type
twoThreeFour' :: [Integer]
twoThreeFour' = map plus1 [1,2,3]

shout :: String -> String
shout x = map toUpper x

-- example of a higher order function
--   What does it do?  
--      takes a fn that takes an Integer and returns a Boolean
--      and a list of Integers, returning all list elements
--      that return True to (f x)
--   How is it called?
--      filterInteger even [1,2,3,4]  - should return [2,4]
filterInteger :: (Integer -> Bool) -> [Integer] -> [Integer]
filterInteger _ []     = []
filterInteger f (x:xs)
  | f x       = x : filterInteger f xs
  | otherwise = filterInteger f xs
  
{-
    RECURSIVE DATA TYPES
    
    Data types can be 'recursive', defined in terms of themselves; 
    List is a recursive data type. We often use recursive functions
    to process recursive data types.
    
    Another example, we can define a binary tree with an Int stored
    at each node and a Char in each leaf.
    
-}
data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show
  
tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

  
