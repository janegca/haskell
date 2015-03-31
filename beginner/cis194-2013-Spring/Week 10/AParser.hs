{-
    Week 10 Applicative Functors - Part 1
        Homework
    
    Ref:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/lectures/
        10-applicative.html
        
-}

module AParser where

import Control.Applicative
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
{-
    Exercise 1
    
    First, you’ll need to implement a Functor instance for Parser.
    Hint: You may find it useful to implement a function
    
        first :: (a -> b) -> (a,c) -> (b,c)
-}    
-- solution: 
-- https://github.com/jroblak/haskell-learnings/blob/
--        master/upenn/hw10/hw10.hs
instance Functor Parser where
    fmap f (Parser a) = Parser $ fmap (first f) . a
    
first :: (a -> b) -> (a,c) -> (b,c)
-- apply a function to the first element of a pair
first f pair = (f $ fst pair, snd pair)   

{-
    Exercise 2
    
    Now implement an Applicative instance for Parser:
    
    • pure a represents the parser which consumes no input and successfully
      returns a result of a.
    
    • p1 <*> p2 represents the parser which first runs p1 (which will
      consume some input and produce a function), then passes the
      remaining input to p2 (which consumes more input and produces
      some value), then returns the result of applying the function to the
      value. However, if either p1 or p2 fails then the whole thing should
      also fail (put another way, p1 <*> p2 only succeeds if both p1 and
      p2 succeed).

-}
-- solution: https://github.com/jdangerx/cis194/10hw/AParser.hs
instance Applicative Parser where
    pure a                      = Parser (\ s -> Just (a, s))
    (Parser f1) <*> (Parser f2) = Parser f
        where
        f [] = Nothing
        f s  = case f1 s of                         -- apply 1st parser
                Nothing       -> Nothing            -- fails
                Just (a', s') -> first a' <$> f2 s' -- apply 2nd parser
                
{-

    Exercise 3

    We can also test your Applicative instance using other simple
    applications of functions to multiple parsers. You should implement
    each of the following exercises using the Applicative interface to put
    together simpler parsers into more complex ones. Do not implement
    them using the low-level definition of a Parser! In other words, 
    pretend that you do not have access to the Parser constructor or even
    know how the Parser type is defined.

    • Create a parser
        abParser :: Parser (Char, Char)
      
      which expects to see the characters ’a’ and ’b’ and returns them
      as a pair. That is,

           *AParser> runParser abParser "abcdef"
            Just ((’a’,’b’),"cdef")
            *AParser> runParser abParser "aebcdf"
            Nothing

    • Now create a parser
        abParser_ :: Parser ()
        
      which acts in the same way as abParser but returns () instead of
      the characters ’a’ and ’b’.
      
        *AParser> runParser abParser_ "abcdef"
        Just ((),"cdef")
        *AParser> runParser abParser_ "aebcdf"
        Nothing

    • Create a parser intPair which reads two integer values separated
      by a space and returns the integer values in a list. You should use
      the provided posInt to parse the integer values.

        *Parser> runParser intPair "12 34"
        Just ([12,34],"")      
        
    [Notes:
        In the functions below, the left side of the <$> operator
        is the function to be applied while the right side is
        a pattern of the expected input
        
        The types of the left-side functions are:
        
            (,) :: a -> b -> (a, b)
            const() :: b -> ()
            (\x _ y -> [x,y]) :: t1 -> t -> t1 -> [t1]
            
        while the types of the results (from the application of the
        functions) are
        
            ex3a :: Maybe ((Char, Char), String)
            ex3b :: Maybe ((), String)
            ex3c :: Maybe ([Integer], String)
    ]
-}         
-- solutions for following 3 functions from:
-- https://github.com/pdswan/cis194/blob/master/hw10/AParser.hs  
--     
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$> abParser

intPair :: Parser [Integer]
intPair = (\x _ y -> [x,y]) <$> posInt <*> char ' ' <*> posInt

s1, s2, s3 :: String
s1 = "abcdef"
s2 = "aebcdf"
s3 = "12 34"

ex3a  = runParser abParser  s1      -- Just (('a','b'),"cdef")
ex3a' = runParser abParser  s2      -- Nothing
ex3b  = runParser abParser_ s1      -- Just ((),"cdef")
ex3b' = runParser abParser_ s2      -- Nothing
ex3c  = runParser intPair   s3      -- Just ([12,34],"")

{-
    Exercise 4

    Applicative by itself can be used to make parsers for simple, fixed
    formats. But for any format involving choice (e.g. “. . . after the
    colon there can be a number or a word or parentheses. . . ”) 
    Applicative is not quite enough. To handle choice we turn to the 
    Alternative class, defined (essentially) as follows:
        
        class Applicative f => Alternative f where
            empty :: f a
            (<|>) :: f a -> f a -> f a
        
    (<|>) is intended to represent choice: that is, f1 <|> f2 represents
    a choice between f1 and f2. empty should be the identity element for
    (<|>), and often represents failure.
    
    Write an Alternative instance for Parser:
  
    • empty represents the parser which always fails.
    • p1 <|> p2 represents the parser which first tries running p1. If
      p1 succeeds then p2 is ignored and the result of p1 is returned.
      Otherwise, if p1 fails, then p2 is tried instead.
    
    Hint: there is already an Alternative instance for Maybe which you
    may find useful.

-}
{- my solution
instance Alternative Parser where
    empty                       = Parser (\_ -> Nothing)
    (Parser p1) <|> (Parser p2) = Parser p
        where p [] = Nothing
              p s  = case p1 s of
                        Nothing -> p2 s     -- failed, run 2nd parser
                        success -> success
-}
-- a cleaner solution from
--      https://github.com/jdangerx/cis194/10hw/AParser.hs
-- Maybe already has an Alternative instance, we can lift (<|>) into
-- the (String ->) context and apply (<|>) to the Maybe (a, String) bits.
instance Alternative Parser where
    empty                       = Parser {runParser = pure Nothing}
    (Parser f1) <|> (Parser f2) = Parser $ (<|>) <$> f1 <*> f2         

{-

    Exercise 5

    Implement a parser
        intOrUppercase :: Parser ()
    
    which parses either an integer value or an uppercase character, and
    fails otherwise.
    
        *Parser> runParser intOrUppercase "342abcd"
        Just ((), "abcd")
        *Parser> runParser intOrUppercase "XYZ"
        Just ((), "YZ")
        *Parser> runParser intOrUppercase "foo"
        Nothing

    Next week, we will use your parsing framework to build a more
    sophisticated parser for a small programming language!    
-}
intOrUppercase :: Parser ()
intOrUppercase =  const () <$> posInt
              <|> const () <$> satisfy isUpper
    
ex5a = runParser intOrUppercase "1235abcd"
ex5b = runParser intOrUppercase "XYZ"
ex5c = runParser intOrUppercase "abcd"
    