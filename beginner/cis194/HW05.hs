{-
    Exercise - Types and Classes
    Source:
        CIS 194: Introduction to Haskell (Fall 2014), Richard Eisenberg
        Week 5 Homework http://www.seas.upenn.edu/~cis194/lectures.html

    Additional files (Parser.hs, Ring.hs) d/ from:
    http://www.seas.upenn.edu/~cis194/extras/05-type-classes/Parser.hs
    http://www.seas.upenn.edu/~cis194/extras/05-type-classes/Ring.hs
 
    Suggested reading (for information on Ring mathematics):
    https://en.wikipedia.org/wiki/Ring_(mathematics)
    
    Note:
        Got stuck on this and had to go googling for help, found it
        in: https://www.cis.upenn.edu/extras/09-testing/Ring.hs
        
    TODO:  this is not complete
    
-}
{-# OPTIONS_GHC -Wall #-}

module HW05 where

import Data.List (stripPrefix)
import Ring
import Parser
{-
    General Info (short version)
    
    Rings
    -----
    
    A 'Ring' is a mathematical structure that obeys certain laws.
    Integers are an example of a ring carrier set (R) with the
    operations addition and multiplication which obey a number o
    of laws i.e. addition is associative, commutative, has and
    additive identity, etc while multiplication is associative,
    distributive over addition, has a multiplicative identity, etc.
    The Ring.hs file contains a type class, Ring, to represent a
    Ring in Haskell, 
    
        class Ring a where
            addId  :: a              -- additive identity
            addInv :: a -> a         -- additive inverse
            mulId  :: a              -- multiplicative identity
            add    :: a -> a -> a    -- addition
            mul    :: a -> a -> a    -- multiplication    
    
    and a defined instance that implements the operations for the
    Integer type:
    
        instance Ring Integer where
            addId  = 0
            addInv = negate
            mulId  = 1
            add    = (+)
            mul    = (*)    

    Now that we have a Ring class, we can write operations that are
    generic with respect to which ring we are operating over,
    
    Parsing
    -------
    One operation we would like to be able to write once for all rings is
    parsing expressions. Any ring supports the idea of expressions like 
    a + (b * c) + d, and so we should be able to write one parser (that
    is, function that converts from a string to a ring object) to produce
    them all.
    
    Parsing is a little tricky and requires more knowledge of Haskell
    than we currently have so the Parse.hs file is provided and includes
    the following class definition:
    
        class Parsable a where
        -- | If successful, ’parse’ returns the thing parsed along with 
        -- the "leftover" string, containing all of the input string 
        -- except what was parsed
        parse :: String -> Maybe (a, String)    
    
    There’s just one problem, though. The parser needs to be able
    to deal with so-called literals: the a, b, c, and d above. 
    For example, if your ring is the integers, literals would look like 
    3 or -2. If your ring is 2 x 2 matrices, though, literals would look 
    more like [[1,2][8,-2]]. How to make the parser generic over different
    literal forms? Use a typeclass!    
    
    You will have to define Parsable instances for any type that you 
    want to be parsed. Here's an example of a Parsable instance for
    the Bool type:
    
        instance Parsable Bool where
          parse str
            | Just rest <- stripPrefix "True" str   = Just (True, rest)    
            | Just rest <- stripPrefix "False" str  = Just (False, rest)
            | otherwise                             = Nothing
            
    which can actually be written as:
    
        instance Parsable Bool where
            parse = listToMaybe . reads
-}
{-  
    Ex 1
    
    Introducing testing (no framework yet, just the basic idea
    of writing tests to check your functions). Every exercise
    in this assignment should have a few definitions to show
    the code works. The function 'intParsingWorks' defined below
    is an example of what's wanted. Fire up GHCi, load this
    file and run 'intParsingWorks', it should return True.
    
    Note that type signatures were added to the numbers (3 :: Integer),
    (11 :: Integer), (0 :: Integer), to be sure the compiler knows 
    we want to test integer's and not Double's, Float's, etc.

    Make sure to include comments explaining how to use your
    testing definitions!
-}
intParsingWorks :: Bool
intParsingWorks =  (parse "3" == Just (3 :: Integer, "")) 
                && (parseRing "1 + 2 * 5" == Just (11 :: Integer)) 
                && (addId == (0 :: Integer))
                
{-
    Ex 2
    
    Modular arithmetic forms a ring. We will be thinking of
    the integers modulo 5. This ring has 5 elements: 
        R = {0, 1, 2, 3, 4}
        
    Addition is like normal integer addition, but it wraps around. So,
    3 + 4 = 2 and 1 + 4 = 0. 
    
    Multiplication is like normal integer multiplication, but it, too, 
    wraps around. Note that Haskell’s mod function is very handy here!
    
    Define a datatype:
    
            data Mod5 = MkMod Integer
                deriving (Show, Eq)
    
    with Ring and Parsable instances. (Your Parsable instance should
    parse just like Integer’s.)
    
    Test your instances!    
    
    Notes (from the Wiki entry):
        Modular arithmetic
            x + y is the remainder of (x + y) div by modular number
            x * y is the remainder of (x * y) div by modular number
            the additive inverse for any value x in the ring is -x
-}                
data Mod5 = MkMod Integer deriving (Show, Eq)

mkMod :: Integer -> Mod5        -- create a Mod5 value
mkMod = MkMod . (`mod` 5)

unMkMod :: Mod5 -> Integer      -- convert a Mod5 value to an Integer
unMkMod (MkMod n) = n

instance Ring Mod5 where
    addId   = MkMod 0
    addInv  = MkMod . negate . unMkMod
    mulId   = MkMod 1
    
    add x y = mkMod (unMkMod x + unMkMod y)
    mul x y = mkMod (unMkMod x * unMkMod y)

-- 'do' is related to monad's, which haven't been covered
-- in the course material as yet    
instance Parsable Mod5 where
  parse str = do (n, rest) <- parse str
                 return (mkMod n, rest)
                 
mod5ParsingWorks :: Bool
mod5ParsingWorks = (parse "3"             == Just (mkMod 3, "")) 
                && (parseRing "3 + 4 * 5" == Just (mkMod 3)) 
                && (addId                 == (mkMod 0))
                
{-
    Exercise 3
    
    Matrix arithmetic forms a ring. Write a data type Mat2x2
    (you choose the representation) and Ring and Parsable instances.
    
    Your parser must be able to read something like [[1,2][3,4]] as a
    2 x 2 matrix. It does not need to allow for the possibility of spaces.
    
    Writing this idiomatically in Haskell is hard, so we will be more
    forgiving about style in the matrix parser.
    Test your instances!
    
    Implementation Notes:
    1. the identity matrix for a 2x2 matrix is
            1 0
            0 1
       or Mat2x2 1 0 0 1
       
    2. Matrix addition is straightforward, corresponding elments
       are added
       
    3. Matrix multiplication; entries equal to sum of
       row * column calculations.

-}       
        
data Mat2x2 = Mat2x2 Integer Integer Integer Integer
    deriving (Eq, Show, Read)
     
instance Ring Mat2x2 where
    addId                   = Mat2x2 0 0 0 0
    addInv (Mat2x2 a b c d) = 
        Mat2x2 (negate a) (negate b) (negate c) (negate d)
    mulId                   = Mat2x2 1 0 0 1
    
    add (Mat2x2 a b c d) (Mat2x2 w x y z) =
        Mat2x2 (a + w) (b + x) (c + y) (d + z)
        
    mul (Mat2x2 a b c d) (Mat2x2 w x y z) =
        Mat2x2 (a * w + b * y)
               (a * x + b * z)
               (c * w + d * y)
               (c * x + d * z)
 
instance Parsable Mat2x2 where
  parse str = do
    str1      <- stripPrefix "[[" str
    (a, str2) <- parse str1
    str3      <- stripPrefix ","  str2
    (b, str4) <- parse str3
    str5      <- stripPrefix "],[" str4
    (c, str6) <- parse str5
    str7      <- stripPrefix ","  str6
    (d, str8) <- parse str7
    str9      <- stripPrefix "]]" str8
    str10     <- stripPrefix " " str9
    return (Mat2x2 a b c d, str10)
    
matParseWorks :: Bool
matParseWorks =  (parse "[[1,2],[3,4]]"   == Just (Mat2x2 1 2 3 4, ""))
              -- && (parse "[[1,2],[3,4]] + [[4,3],[2,1]]" ==
              --    Just (Mat2x2 5 5 5 5, ""))

{-
    Problem
    
    *HW05> parse ms2 :: Maybe (Mat2x2, String)
    Just (Mat2x2 1 2 3 4," + [[4,3],2,1]]")
    
    the 'do' is stopping after building on matrix, 
    how to continue??
    
    Check out tutorial at
    https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec
    
    - make a matParser that creates an Mat2x2
    - parser then becomes
    parse str =   many $ matParser
-}
              
                  
m1 :: Mat2x2
m1 = Mat2x2 1 2 3 4

m2 :: Mat2x2
m2 = Mat2x2 4 3 2 1

ms1 :: String
ms1 = "[[1,2],[3,4]]"
                 
ms2 :: String
ms2 = "[[1,2],[3,4]] + [[4,3],2,1]]"
               
