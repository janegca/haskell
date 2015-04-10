{-# OPTIONS -fno-warn-type-defaults -XMultiParamTypeClasses #-}
module Classes where
{-
    CIS 552: Advanced Programming (2015)
    Lecture 5 - Typeclasses
    
    Source:
    https://www.seas.upenn.edu/~cis552/lectures/stub/Classes.html

-}

import Test.HUnit
import Data.Char
import Prelude hiding (lookup)

{-
    Qualified types have 'type constraints'
    
    i.e. the type signature of (+) is:
            Num a => a -> a -> a
         where
            Num a =>  is a 'type constraint' that signals
           (+) will work for any type 'a' as long as it is a
           member of the typeclass Num
           
    Another function with a constraint is (==)
        Eq a => a -> a -> a
        where
            Eq a => is a type contraint that signals (==)
            will work for any type 'a' as long as it is a
            member of the typeclass Eq
            
    The Eq class is defined as:
    
        class Eq a where
          (==) :: a -> a -> Bool
          (/=) :: a -> a -> Bool    
          
          x /= y               = not (x == y)
          x == y               = not (x /= y)
          
    where class Eq a declares Eq as a typeclass with a single
          parameter, 'a'
          Eq provides default implementations for its two
          methods (functions)
          
    To add a type to the Eq type class, we need to define at
    least one of the two functions (==) and (/=) in an 'instance' 
    declaration
            
-}
-- a new data type
data PrimaryColor = Red | Green | Blue

-- add PrimaryColor to the Eq typeclass
instance Eq PrimaryColor where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False
    
-- now we can compare PrimayColor's
fancyTrue :: Bool
fancyTrue = Red == Red    

-- another example
data Tree a = Leaf | Branch a (Tree a) (Tree a)

-- we can only compare Trees if the values they hold are also
-- comparable so the type, 'a', must be constrained to Eq
instance Eq a => Eq (Tree a) where
    Leaf           == Leaf           = True
    Branch a a1 a2 == Branch b b1 b2 = a == b && a1 == b1 && a2 == b2
    _              == _              = False
    
tree1, tree2 :: Tree Int
tree1 = Branch 3 (Branch 2 Leaf Leaf) (Branch 1 Leaf Leaf)
tree2 = Branch 3 Leaf Leaf

testTreeEq :: Test
testTreeEq = TestList [ "tree1 == tree1" ~: tree1 == tree1 ~?= True,
                        "tree1 /= tree2" ~: tree1 == tree2 ~?= False,
                        "tree1 /= Leaf"  ~: tree1 == Leaf  ~?= False ]
                        
runTestTEq = runTestTT testTreeEq
                        
{-
    Typeclass constraints can appear on any function; not just
    functions defined as typeclass members. 
    For example, the 'lookup' function in the standard library
    has the following definition:
    
        lookup :: Eq a => a -> [(a,b)] -> Maybe b
        lookup _ []          = Nothing
        lookup a ((a',b):ps) = if a == a' then Just b 
                                          else lookup a ps   

    Because we need to compare the 'key' values in a list of (key,value)
    pairs, any type 'a' used as a key values MUST belong to the Eq
    typeclass and we make this explicit by adding the 'Eq a =>'
    constraint to the function type signature.

-}    
-- writing boilerplate (==) and/or (/=) functions for our classes
-- can be tedious so Haskell provides a 'deriving' mechanism to
-- automate this for us as long as the types used in our data
-- constructors belong to Eq
-- In theses examples, Double and Float are members of Eq so
-- deriving Eq for Point works fine and, as we now have a way
-- to compare Points, deriving Eq for Shape also works
data Point = Point Double Double
    deriving (Eq)
    
data Shape = Circle    Point Float
           | Rectangle Point Point
    deriving (Eq)
    
-- deriving Eq for a type that uses functions in its data
-- constructors will NOT work as there is no way to compare
-- functions
data IntFunctions = OneArg (Int -> Int)
                  | TwoArg (Int -> Int -> Int)
    -- deriving Eq  -- will cause an error
    
{-  GHC can derive instances for a number of built-in typeclasses
   (Eq, Show, Read, etc) but NOT all of them
  
    The Show typeclass converts data to a String so it can be displayed
    The show function has the following type signature:
   
        show :: Show a => a -> String
        
    and the class has the following declaration:
   
        class Show a where
          show      :: a   -> String

          showsPrec :: Int -> a -> ShowS
          showList  :: [a] -> ShowS   
          
    To define an instance of Show, implement show or showsPrec (which
    takes an Int to indicate printing precedence [?? find an example]
    'showList' can be used to specify a special way of printing
    list values.
    
    Usually 'deriving Show' produces adequate String results.
    
    Read is another built-in typeclass for which GHC can derive
    instances. The 'read' function has the following signature:
    
        read :: Read a => String -> a
        
    it takes a String and returns a value.
    i.e. entering read "3" :: Int at a ghci prompt will return 3
-}  
data SadColors = Black | Brown | Grey
   deriving (Eq, Show, Read)
   
-- typeclasses can have multiple constraints
--      class (Read a, Show a) => Serializable a where
--          toFile :: a -> ByteString
--          ...   
--
-- and multiple parameters (must use '-XMultiParamTypeClasses' flag)

class Convertible a b where
    convert :: a -> b       -- how to convert from one type to another
    
instance Convertible Char Int where
    convert = ord 

instance Convertible (Tree a) [a] where
    convert = infixOrder where
       infixOrder Leaf           = []
       infixOrder (Branch v l r) = infixOrder l ++ [v] ++ infixOrder r  

{-
    Another built-in typeclass is Ord, which is a typeclass for things
    that can be ordered
    
        class Eq a => Ord a where
           compare              :: a -> a -> Ordering
           (<), (<=), (>), (>=) :: a -> a -> Bool
           max, min             :: a -> a -> a    
           
    any type that is a member of Ord must also be a member of Eq
    
    The 'compare' method makes use of another built-in type, Ordering
    
        data Ordering = LT | EQ | GT
        
    compare takes two arguments and tells us whether the first is
    LT (<), EQ (==), or GT (>) the second argument.
    
    Ord can also be derived; if you need to write your own instance,
    you need only provide an implementation for 'compare' of (<=)

-}       
{-
    Enum and Bounded are two other built-in typeclasses that allow
    us to easily create ordered lists i.e. [1..10], ['a'..'z']
    
    Enum lets us create sequentially ordered lists
    
        class Enum a  where
          succ, pred           :: a -> a

          toEnum               :: Int -> a
          fromEnum             :: a -> Int

          -- These are used in haskell's translation of [n..m]
          enumFrom            :: a -> [a]
          enumFromThen        :: a -> a -> [a]
          enumFromTo          :: a -> a -> [a]
          enumFromThenTo      :: a -> a -> a -> [a]    
          
    Bounded allows us to tell how big things are
    
        class Bounded a where
            minBound, maxBound     :: a
            
    i.e. maxBound :: Int will return how large an Int is on your
         system
        
    Not all built-in types support Enum and Bounded; those that do,
    do not take parameters.

-}
{-
    Functor Typeclass
    -----------------
    
    Is a typeclass for structures that we can map a function over.
    
        class Functor f where
            fmap :: (a -> b) -> f a -> f b
    
    Functor is a 'constructor class'; it works on constructors, like
    Tree, Maybe, [] i.e. constructors that take an argument. The
    [] Functor instance just equates fmap to map.
    
        instance Functor [] where
          -- fmap :: (a -> b) -> [a] -> [b]
          fmap = map    
    
    The Functor instance for Maybe is declared as:
    
        instance Functor Maybe where
           fmap _ Nothing  = Nothing
           fmap f (Just a) = Just (f a)    
    
    We can define a Functor instance for our own data types

-}
instance Functor Tree where
  fmap = treeMap where
    treeMap f Leaf = Leaf
    treeMap f (Branch x l r) = Branch (f x) (treeMap f l) (treeMap f r)
    
{-
    Monad Typeclass
    ---------------

    The Monad typeclass has the following declaration:
    
        class  Monad m  where

          -- | Sequentially compose two actions, passing any value 
          -- produced by the first as an argument to the second.
          (>>=)       :: m a -> (a -> m b) -> m b

          -- | Inject a value into the monadic type.
          return      :: a -> m a

          -- | Sequentially compose two actions, discarding any value 
          -- produced by the first, like sequencing operators (such as 
          -- the semicolon) in imperative languages.
          (>>)        :: m a -> m b -> m b
          m >> k      = m >>= \_ -> k         -- default definition

          -- | Fail with a message.  This operation is not part of the
          -- mathematical definition of a monad, but is invoked on 
          -- pattern-match failure in a @do@ expression.
          fail        :: String -> m a
          fail s      = error s              -- default definition

    Note that the 'do' notation is syntactic sugar for the (>>=)
    sequence operator.
    
        do 
              x <- doSomething
              doSomethingElse
              y <- andSoOn
              return ()    

    is shorthand for:
    
        doSomething >>= ( \x -> 
             doSomethingElse >>
               (andSoOn >>= ( \y -> 
                 return () )))    
-}    